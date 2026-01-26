# Lake GitHub Release Artifacts Guide

This guide explains how to distribute pre-built Lean artifacts via GitHub releases, so downstream users don't need to rebuild your library from source.

## Why Use Release Artifacts?

For libraries with complex proofs or long compile times, providing pre-built artifacts:
- Reduces build time for downstream users from minutes to seconds
- Eliminates the need for users to have the exact build toolchain
- Makes your library more accessible to end users

## Configuration

### For the Library (Producer)

Add these fields to your `lakefile.toml`:

```toml
name = "lean-uri"
version = "0.1.0"
defaultTargets = ["LeanUri"]
testDriver = "testURI"

# Cloud release configuration
releaseRepo = "https://github.com/yourusername/lean-uri"
buildArchive = "lean-uri-{System.Platform.target}.tar.gz"  # optional, this is the default
```

**Configuration fields:**

- `releaseRepo` - The GitHub repository URL where releases are hosted. If omitted, Lake uses the URL from which the package was cloned.
- `buildArchive` - Custom name for the build archive. Default is `{package-name}-{platform}.tar.gz`
- `preferReleaseBuild` - Set to `true` in downstream packages to prefer downloading pre-built artifacts

### For Downstream Packages (Consumers)

In a package that depends on `lean-uri`, add to the dependency configuration:

```toml
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "v0.1.0"  # or a specific commit/tag
preferReleaseBuild = true  # This tells Lake to download pre-built artifacts
```

**Important:** Lake only fetches release builds for dependencies, not for the root package (since you're presumably editing the root package).

## Workflow

### 1. Build Your Library

```bash
# Build all artifacts
lake build

# This creates files in .lake/build/:
# - .lake/build/lib/*.olean (compiled Lean files)
# - .lake/build/lib/*.a (static libraries)
# - .lake/build/ir/*.c (generated C code)
```

### 2. Pack the Build Artifacts

```bash
# Pack the build directory into an archive
lake pack

# This creates .lake/{package-name}-{platform}.tar.gz
# For example: .lake/lean-uri-x86_64-unknown-linux-gnu.tar.gz
```

The `lake pack` command:
- Uses `tar` to create a gzipped archive
- Only packs existing artifacts (doesn't build)
- Places the archive in `.lake/` by default

### 3. Create a GitHub Release

```bash
# First, create a tag
git tag v0.1.0
git push origin v0.1.0

# Create the GitHub release (using gh CLI)
gh release create v0.1.0 --title "Version 0.1.0" --notes "Release notes here"
```

### 4. Upload the Artifact

```bash
# Upload the packed artifact to the release
lake upload v0.1.0

# Or manually with gh:
gh release upload v0.1.0 .lake/lean-uri-*.tar.gz
```

The `lake upload` command:
- Uses `tar` to pack the build directory
- Uses `gh release upload` to attach it to the specified tag
- Requires `gh` (GitHub CLI) to be installed and authenticated

## Testing Locally Without Creating a Real Release

To test the artifact workflow without polluting your actual releases:

### Option 1: Local Pack/Unpack Test

```bash
# 1. Build your library
lake build

# 2. Pack it
lake pack

# 3. Clean the build directory
rm -rf .lake/build

# 4. Unpack it
lake unpack

# 5. Verify the artifacts are present
ls .lake/build/lib/
```

### Option 2: Test with a Development Release

```bash
# Create a test tag
git tag v0.0.0-test
git push origin v0.0.0-test

# Create a draft or pre-release
gh release create v0.0.0-test --draft --title "Test Release"

# Upload the artifact
lake upload v0.0.0-test

# Test downloading in a consumer project
# (use v0.0.0-test as the rev)

# Clean up when done
gh release delete v0.0.0-test --yes
git tag -d v0.0.0-test
git push origin :refs/tags/v0.0.0-test
```

### Option 3: Test with a Separate Test Repository

Create a separate test repository to experiment with the full workflow without affecting your main project.

## Downstream Consumption

When a downstream package depends on your library with `preferReleaseBuild = true`:

```toml
# In downstream-project/lakefile.toml
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "v0.1.0"
preferReleaseBuild = true
```

Then when they run `lake update`:

```bash
lake update

# Lake will:
# 1. Clone the dependency repository
# 2. Check for a release matching the revision
# 3. Download the platform-specific artifact (using curl)
# 4. Unpack it into .lake/packages/lean-uri/.lake/build/ (using tar)
# 5. Skip building from source if the artifact is present
```

**Important notes:**
- Lake requires `curl` and `tar` to be installed (for downloading)
- The archive name must match the platform: `{package}-{System.Platform.target}.tar.gz`
- If download fails, Lake falls back to building from source
- Release builds are only used for dependencies, never for the root package

## Platform-Specific Artifacts

Lake's build artifacts are platform-specific. Common platforms:

- `x86_64-unknown-linux-gnu` - Linux x86_64
- `x86_64-apple-darwin` - macOS Intel
- `aarch64-apple-darwin` - macOS Apple Silicon
- `x86_64-pc-windows-msvc` - Windows

For full cross-platform support, you'd need to build and upload artifacts for each platform:

```bash
# On Linux
lake build && lake pack
gh release upload v0.1.0 .lake/lean-uri-x86_64-unknown-linux-gnu.tar.gz

# On macOS Intel
lake build && lake pack
gh release upload v0.1.0 .lake/lean-uri-x86_64-apple-darwin.tar.gz

# On macOS ARM
lake build && lake pack
gh release upload v0.1.0 .lake/lean-uri-aarch64-apple-darwin.tar.gz
```

This can be automated with GitHub Actions CI.

## Disabling Release Downloads

Users can disable release artifact downloads by setting:

```bash
export LAKE_NO_CACHE=1
lake update
```

## Summary of Commands

```bash
# Producer (library maintainer)
lake build                    # Build all artifacts
lake pack [archive.tar.gz]   # Pack build directory
lake upload <tag>             # Upload to GitHub release

# Consumer (library user)
lake update                   # Downloads artifacts if preferReleaseBuild = true

# Testing
lake unpack [archive.tar.gz] # Unpack artifacts into build directory
```

## Complete Example Workflow

```bash
# As the library maintainer:
cd lean-uri
lake build                          # Build everything
lake pack                           # Create the archive
git tag v0.2.0
git push origin v0.2.0
gh release create v0.2.0 --title "Version 0.2.0"
lake upload v0.2.0                 # Upload the artifact

# As a downstream user:
cd my-project
# Edit lakefile.toml to add:
#   [[require]]
#   name = "lean-uri"
#   git = "https://github.com/yourusername/lean-uri"
#   rev = "v0.2.0"
#   preferReleaseBuild = true

lake update                         # Downloads the pre-built artifact
lake build                          # Builds your project (using pre-built lean-uri)
```

## Troubleshooting

**"curl not found" or "tar not found"**
- Install these tools on your system

**"Release not found"**
- Verify the release exists: `gh release view <tag>`
- Check that the tag matches exactly

**"Archive not found in release"**
- Check the archive exists: `gh release view <tag>`
- Verify the platform matches

**Artifacts not being used**
- Ensure `preferReleaseBuild = true` is set in the dependency configuration
- Remember: release builds only work for dependencies, not root packages
- Check if `LAKE_NO_CACHE=1` is set (disables downloads)

**Wrong platform artifact**
- Lake automatically selects based on `System.Platform.target`
- Upload artifacts for each platform you want to support

## Advanced: GitHub Actions Automation

Consider automating this with GitHub Actions to build and upload artifacts for all platforms on release:

```yaml
name: Release Artifacts
on:
  release:
    types: [created]
jobs:
  build-and-upload:
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, macos-14]  # macos-14 is ARM
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v3
      - uses: leanprover/lean-action@v1
      - run: lake build
      - run: lake pack
      - run: gh release upload ${{ github.ref_name }} .lake/*.tar.gz
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

This ensures all platforms have artifacts whenever you create a release.
