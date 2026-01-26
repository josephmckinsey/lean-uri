# Lake Release Artifacts Guide

Pre-built artifacts let downstream users skip rebuilding your library from source, reducing build times from minutes to seconds.

## TL;DR

**If you maintain a library:**
```bash
lake build && lake pack && lake upload v1.0.0
```

**If you use a library with artifacts:**
```toml
[[require]]
name = "some-library"
git = "https://github.com/org/some-library"
rev = "v1.0.0"
preferReleaseBuild = true  # Add this line
```

---

# Producer Guide

> **Audience:** Library maintainers who want to provide pre-built artifacts

## Why Provide Artifacts?

For libraries with complex proofs or long compile times:
- **Users benefit:** Build times drop from minutes/hours to seconds
- **You benefit:** Fewer "build too slow" complaints, more adoption
- **Everyone benefits:** Lower CI costs, faster iteration

## Configuration

Add to your `lakefile.toml`:

```toml
name = "my-library"
version = "0.1.0"

# Release artifact configuration
releaseRepo = "https://github.com/yourusername/my-library"
```

That's it. The `buildArchive` field is optional (defaults to `{name}-{platform}.tar.gz`).

> **Note:** The `releaseRepo` can be omitted if your package will always be cloned from that URL, but it's safer to be explicit.

## Testing Locally (Before Publishing)

Test the pack/unpack workflow without creating GitHub releases:

```bash
# 1. Build your library
lake build

# 2. Pack the build artifacts
lake pack
# Creates: .lake/my-library-x86_64-unknown-linux-gnu.tar.gz

# 3. Simulate a fresh download
rm -rf .lake/build

# 4. Unpack the archive
lake unpack

# 5. Verify it works
lake build  # Should be instant since artifacts exist
```

Or use the included test script:
```bash
./test-artifacts.sh
```

## Publishing Workflow

Once you're ready to publish:

### 1. Build and Pack

```bash
lake build  # Build all artifacts
lake pack   # Pack into .lake/{name}-{platform}.tar.gz
```

### 2. Create GitHub Release

```bash
git tag v0.1.0
git push origin v0.1.0
gh release create v0.1.0 --title "Version 0.1.0" --notes "Release notes here"
```

### 3. Upload Artifacts

```bash
lake upload v0.1.0
```

This runs `gh release upload` to attach the archive to your release.

**Requirements:** You need `gh` (GitHub CLI) installed and authenticated.

## Platform-Specific Builds

> **Important:** Build artifacts are platform-specific!

Common platforms:
- `x86_64-unknown-linux-gnu` - Linux x86_64
- `x86_64-apple-darwin` - macOS Intel
- `aarch64-apple-darwin` - macOS Apple Silicon
- `x86_64-pc-windows-msvc` - Windows

**For full cross-platform support**, build and upload on each platform:

```bash
# On Linux
lake build && lake pack
gh release upload v0.1.0 .lake/my-library-x86_64-unknown-linux-gnu.tar.gz

# On macOS Intel
lake build && lake pack
gh release upload v0.1.0 .lake/my-library-x86_64-apple-darwin.tar.gz

# On macOS ARM
lake build && lake pack
gh release upload v0.1.0 .lake/my-library-aarch64-apple-darwin.tar.gz
```

See "Advanced: CI/CD Automation" below for automating this.

## Command Reference

```bash
lake pack [archive.tar.gz]   # Pack .lake/build/ into archive
lake unpack [archive.tar.gz] # Unpack archive into .lake/build/
lake upload <tag>            # Pack and upload to GitHub release
```

> **Important:** `lake pack` only packs existing artifacts - it doesn't build them. Always run `lake build` first.

---

# Consumer Guide

> **Audience:** Developers using libraries that provide pre-built artifacts

## Configuration

Add one line to your dependency:

```toml
[[require]]
name = "some-library"
git = "https://github.com/org/some-library"
rev = "v1.0.0"
preferReleaseBuild = true  # Add this
```

## What Happens

When you run `lake update`:

```bash
$ lake update

info: downloading https://github.com/org/some-library/releases/download/v1.0.0/some-library-x86_64-unknown-linux-gnu.tar.gz
info: unpacking some-library-x86_64-unknown-linux-gnu.tar.gz
```

Lake:
1. Clones the repository to `.lake/packages/some-library/`
2. Downloads the platform-specific archive from GitHub releases
3. Unpacks it into `.lake/packages/some-library/.lake/build/`
4. Skips building the dependency

Then `lake build` only builds *your* code:

```bash
$ lake build

✔ [1/3] Built MyProject.Main
✔ [2/3] Built MyProject
✔ [3/3] Built my-exe
Build completed successfully (3 jobs).
```

No mention of building the dependency - those artifacts were pre-built!

## Build Time Comparison

**Without artifacts:**
```bash
$ lake build
✔ [1/15] Built SomeLibrary.Defs      # Building dependency...
✔ [2/15] Built SomeLibrary.Core      # Still building...
✔ [3/15] Built SomeLibrary.Proofs    # Heavy proofs...
...
✔ [13/15] Built MyProject.Main       # Finally your code
```
Time: Minutes to hours (for proof-heavy libraries)

**With artifacts:**
```bash
$ lake build
✔ [1/3] Built MyProject.Main
✔ [2/3] Built MyProject
```
Time: Seconds

## Fallback Behavior

If artifact download fails, Lake automatically falls back to building from source:

```
error: failed to download [...]/some-library-x86_64-unknown-linux-gnu.tar.gz
info: falling back to building from source
```

**Common reasons:**
- Release tag doesn't exist
- Archive not uploaded for your platform
- Network issues

**You still get a working build** - just slower.

## Disabling Artifacts

To force building from source:

```bash
# Temporary
LAKE_NO_CACHE=1 lake update && lake build

# Permanent: remove preferReleaseBuild from lakefile.toml
```

## Best Practices

✓ **Use tagged releases** for `rev` (e.g., `v1.0.0`, not `main`)
- Artifacts are uploaded to releases, not arbitrary commits

✓ **Commit your manifest** (`lake-manifest.json`)
- Ensures reproducible builds across your team

✓ **Check release exists** before depending on it
```bash
gh release view v1.0.0 --repo org/some-library
```

---

# Advanced Topics

## Automating Multi-Platform Builds with GitHub Actions

Create `.github/workflows/release.yml`:

```yaml
name: Build and Upload Release Artifacts

on:
  release:
    types: [created]

jobs:
  build-artifacts:
    strategy:
      matrix:
        include:
          - os: ubuntu-latest
            platform: x86_64-unknown-linux-gnu
          - os: macos-13  # Intel
            platform: x86_64-apple-darwin
          - os: macos-14  # ARM
            platform: aarch64-apple-darwin

    runs-on: ${{ matrix.os }}

    steps:
      - uses: actions/checkout@v4

      - uses: leanprover/lean-action@v1

      - name: Build library
        run: lake build

      - name: Pack artifacts
        run: lake pack

      - name: Upload to release
        run: gh release upload ${{ github.ref_name }} .lake/*.tar.gz
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

Now creating a release automatically builds and uploads artifacts for all platforms.

## Understanding Platform Detection

Lake automatically detects the platform using `System.Platform.target`. You can check yours:

```bash
$ lean --run -c "IO.println System.Platform.target"
x86_64-unknown-linux-gnu
```

The archive name must match: `{package}-{System.Platform.target}.tar.gz`

## Custom Archive Names

If you need custom naming (rare):

```toml
buildArchive = "my-custom-name.tar.gz"
```

But this is usually unnecessary - the default is platform-aware.

## Multiple Dependencies

Each dependency with `preferReleaseBuild = true` downloads independently:

```toml
[[require]]
name = "library-a"
git = "https://github.com/org/library-a"
rev = "v1.0.0"
preferReleaseBuild = true

[[require]]
name = "library-b"
git = "https://github.com/org/library-b"
rev = "v2.0.0"
preferReleaseBuild = true
```

Total download time is usually under 30 seconds, regardless of library complexity.

## Artifact Contents

The archive contains the entire `.lake/build/` directory:

```
.lake/build/
├── lib/           # .olean files, .a static libraries
├── ir/            # .c generated C code
└── bin/           # Executables (if any)
```

Trace files are also included, enabling incremental builds.

## Requirements

**For producers (uploading):**
- `tar` (pack archives)
- `gh` (GitHub CLI, authenticated)

**For consumers (downloading):**
- `curl` (download archives)
- `tar` (unpack archives)

Both are standard on most systems.

## Limitations

> **Critical:** Lake only fetches release builds for **dependencies**, never for the **root package**.

Why? The root package is what you're actively editing. If you want to use a release build for your root package (e.g., after cloning), manually run:

```bash
lake build :release
```

This is rarely needed in practice.

## Troubleshooting

**Artifact not being used despite `preferReleaseBuild = true`**
- Check you're not building the root package (artifacts only work for dependencies)
- Verify the release and archive exist: `gh release view <tag> --repo <org/repo>`
- Check platform matches
- Ensure `LAKE_NO_CACHE` is not set

**"tar: command not found"**
- Install tar: `apt install tar` / `brew install tar`

**"gh: command not found"** (when uploading)
- Install GitHub CLI: https://cli.github.com/

**Archive corrupted**
- Delete and retry: `rm -rf .lake/packages/<name> && lake update`

---

## Quick Reference

| Role | Command | Purpose |
|------|---------|---------|
| Producer | `lake build` | Build all artifacts |
| Producer | `lake pack` | Create archive from .lake/build/ |
| Producer | `lake upload <tag>` | Upload to GitHub release |
| Producer | `lake unpack` | Test: unpack archive locally |
| Consumer | Add `preferReleaseBuild = true` | Enable artifact downloads |
| Consumer | `lake update` | Download dependencies & artifacts |
| Both | `LAKE_NO_CACHE=1` | Disable artifact caching |

---

**Further Reading:**
- [Lake Documentation](https://lean-lang.org/doc/reference/latest/Build-Tools-and-Distribution/Lake/)
- [GitHub CLI Docs](https://cli.github.com/manual/)
