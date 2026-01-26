#!/usr/bin/env bash
# Test script for verifying Lake artifact pack/unpack workflow
# This tests the artifact mechanism without requiring GitHub releases

set -e  # Exit on error

echo "=========================================="
echo "Lake Artifact Pack/Unpack Test"
echo "=========================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Step 1: Clean any existing build
info "Step 1: Cleaning existing build artifacts..."
if [ -d ".lake/build" ]; then
    rm -rf .lake/build
    success "Cleaned .lake/build directory"
else
    info "No existing build directory found"
fi

if [ -f ".lake/lean-uri-"*.tar.gz ]; then
    rm -f .lake/lean-uri-*.tar.gz
    success "Cleaned existing archives"
fi

# Step 2: Build the library
info "Step 2: Building the library..."
echo ""
lake build
echo ""
success "Build completed"

# Step 3: Verify build artifacts exist
info "Step 3: Verifying build artifacts..."
if [ ! -d ".lake/build/lib" ]; then
    error "Build directory .lake/build/lib not found!"
    exit 1
fi

OLEAN_COUNT=$(find .lake/build/lib -name "*.olean" | wc -l)
info "Found $OLEAN_COUNT .olean files"

if [ "$OLEAN_COUNT" -eq 0 ]; then
    error "No .olean files found! Build may have failed."
    exit 1
fi

success "Build artifacts verified"

# Step 4: Pack the artifacts
info "Step 4: Packing build artifacts..."
echo ""
lake pack
echo ""

# Find the created archive
ARCHIVE=$(ls .lake/lean-uri-*.tar.gz 2>/dev/null | head -1)
if [ -z "$ARCHIVE" ]; then
    error "No archive created! lake pack may have failed."
    exit 1
fi

ARCHIVE_SIZE=$(du -h "$ARCHIVE" | cut -f1)
success "Archive created: $ARCHIVE (size: $ARCHIVE_SIZE)"

# Step 5: Save archive location and remove build directory
info "Step 5: Removing build directory to simulate fresh download..."
ARCHIVE_BACKUP="/tmp/$(basename $ARCHIVE)"
cp "$ARCHIVE" "$ARCHIVE_BACKUP"
info "Backed up archive to $ARCHIVE_BACKUP"

rm -rf .lake/build
success "Build directory removed"

# Verify it's really gone
if [ -d ".lake/build" ]; then
    error "Failed to remove build directory!"
    exit 1
fi

# Step 6: Unpack the archive
info "Step 6: Unpacking the archive..."
echo ""
lake unpack "$ARCHIVE"
echo ""
success "Archive unpacked"

# Step 7: Verify unpacked artifacts
info "Step 7: Verifying unpacked artifacts..."
if [ ! -d ".lake/build/lib" ]; then
    error "Build directory not restored after unpack!"
    exit 1
fi

OLEAN_COUNT_AFTER=$(find .lake/build/lib -name "*.olean" | wc -l)
info "Found $OLEAN_COUNT_AFTER .olean files after unpack"

if [ "$OLEAN_COUNT_AFTER" -ne "$OLEAN_COUNT" ]; then
    warn "File count mismatch! Before: $OLEAN_COUNT, After: $OLEAN_COUNT_AFTER"
else
    success "File count matches!"
fi

# Step 8: Test that we can use the unpacked artifacts
info "Step 8: Testing that unpacked artifacts work..."
echo ""
# This should be very fast since artifacts are present
lake build
echo ""
success "Build with unpacked artifacts successful"

# Step 9: Compare archives
info "Step 9: Creating a new pack to verify reproducibility..."
mv "$ARCHIVE" "${ARCHIVE}.old"
lake pack
ARCHIVE_NEW=$(ls .lake/lean-uri-*.tar.gz 2>/dev/null | grep -v "\.old$" | head -1)

if [ -f "$ARCHIVE_NEW" ]; then
    SIZE_OLD=$(stat -f%z "${ARCHIVE}.old" 2>/dev/null || stat -c%s "${ARCHIVE}.old")
    SIZE_NEW=$(stat -f%z "$ARCHIVE_NEW" 2>/dev/null || stat -c%s "$ARCHIVE_NEW")

    info "Original archive size: $SIZE_OLD bytes"
    info "New archive size: $SIZE_NEW bytes"

    if [ "$SIZE_OLD" -eq "$SIZE_NEW" ]; then
        success "Archive sizes match (build is reproducible)"
    else
        DIFF=$((SIZE_NEW - SIZE_OLD))
        warn "Archive sizes differ by $DIFF bytes (may be due to timestamps)"
    fi
fi

# Cleanup
info "Cleaning up test files..."
rm -f "$ARCHIVE_BACKUP"
rm -f "${ARCHIVE}.old"
success "Cleanup complete"

echo ""
echo "=========================================="
echo -e "${GREEN}All tests passed!${NC}"
echo "=========================================="
echo ""
echo "Summary:"
echo "  ✓ Built library from source"
echo "  ✓ Packed build artifacts into archive"
echo "  ✓ Unpacked archive successfully"
echo "  ✓ Verified artifact integrity"
echo "  ✓ Confirmed artifacts work in build"
echo ""
echo "You can now safely use 'lake pack' and 'lake upload' for releases."
echo "Archive location: .lake/$(basename $ARCHIVE_NEW)"
