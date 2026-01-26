# Downstream Package Example

This document shows exactly how a downstream package would consume pre-built artifacts from `lean-uri`.

## Scenario

You have a project that depends on `lean-uri` and you want to use pre-built artifacts instead of building from source.

## Directory Structure

```
my-uri-app/
├── lakefile.toml
├── lean-toolchain
├── MyUriApp/
│   └── Basic.lean
└── Main.lean
```

## Configuration

### lakefile.toml

```toml
name = "my-uri-app"
version = "0.1.0"
defaultTargets = ["MyUriApp"]

# Dependency on lean-uri with pre-built artifacts enabled
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "v0.1.0"  # Must match a GitHub release tag
preferReleaseBuild = true  # Key setting: fetch pre-built artifacts

[[lean_lib]]
name = "MyUriApp"

[[lean_exe]]
name = "my-uri-app"
root = "Main"
```

### Main.lean

```lean
import LeanUri

def main : IO Unit := do
  -- Example using the lean-uri library
  let uri := "https://example.com/path?query=value"
  match LeanUri.parseURI uri with
  | some parsed => IO.println s!"Parsed URI successfully: {parsed.scheme}"
  | none => IO.println "Failed to parse URI"
```

## User Experience

### First Time Setup

When a user clones your project and runs `lake update`:

```bash
$ git clone https://github.com/yourusername/my-uri-app
$ cd my-uri-app
$ lake update

# Output:
info: downloading https://github.com/yourusername/lean-uri/releases/download/v0.1.0/lean-uri-x86_64-unknown-linux-gnu.tar.gz
info: unpacking lean-uri-x86_64-unknown-linux-gnu.tar.gz
info: cloning https://github.com/yourusername/lean-uri to ./.lake/packages/lean-uri
```

**What happened:**
1. Lake cloned the `lean-uri` repository to `.lake/packages/lean-uri/`
2. Lake detected `preferReleaseBuild = true`
3. Lake downloaded the pre-built archive from GitHub releases
4. Lake unpacked the archive into `.lake/packages/lean-uri/.lake/build/`
5. No build of `lean-uri` occurred!

### Building the Project

```bash
$ lake build

# Output:
✔ [1/3] Built MyUriApp.Basic
✔ [2/3] Built MyUriApp
✔ [3/3] Built my-uri-app
Build completed successfully (3 jobs).
```

**Notice:**
- Only 3 build jobs (your code)
- No mention of building LeanUri, LeanUri.Defs, LeanUri.Parsers, etc.
- The build is fast because `lean-uri` was pre-built!

### Directory Structure After Update

```
my-uri-app/
├── .lake/
│   ├── packages/
│   │   └── lean-uri/              # The dependency
│   │       ├── LeanUri/           # Source code
│   │       │   ├── Basic.lean
│   │       │   ├── Defs.lean
│   │       │   └── ...
│   │       ├── .lake/
│   │       │   └── build/         # Pre-built artifacts (downloaded!)
│   │       │       ├── lib/
│   │       │       │   ├── LeanUri.olean
│   │       │       │   ├── LeanUri/Basic.olean
│   │       │       │   └── ...
│   │       │       └── ir/
│   │       └── lakefile.toml
│   └── build/                     # Your project's build artifacts
│       ├── lib/
│       │   └── MyUriApp.olean
│       └── bin/
│           └── my-uri-app
├── MyUriApp/
│   └── Basic.lean
├── Main.lean
└── lakefile.toml
```

## Comparison: With vs Without Release Artifacts

### Without Release Artifacts (Building from Source)

```toml
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "v0.1.0"
# preferReleaseBuild not set (defaults to false)
```

```bash
$ lake build

# Output:
✔ [1/10] Built LeanUri.Defs
✔ [2/10] Built LeanUri.Parsers
✔ [3/10] Built LeanUri.IPv4
✔ [4/10] Built LeanUri.IPv6
✔ [5/10] Built LeanUri.URI
✔ [6/10] Built LeanUri.Normalization
✔ [7/10] Built LeanUri.Basic
✔ [8/10] Built LeanUri
✔ [9/10] Built MyUriApp.Basic
✔ [10/10] Built MyUriApp
✔ [11/11] Built my-uri-app
Build completed successfully (11 jobs).
```

**Time:** ~2-5 seconds (for lean-uri) + your build time

### With Release Artifacts

```toml
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "v0.1.0"
preferReleaseBuild = true
```

```bash
$ lake build

# Output:
✔ [1/3] Built MyUriApp.Basic
✔ [2/3] Built MyUriApp
✔ [3/3] Built my-uri-app
Build completed successfully (3 jobs).
```

**Time:** ~0.5 seconds (just your code)

### Impact for Complex Libraries

For a library with heavy proofs (like mathlib4):
- **Without artifacts:** 30-60+ minutes
- **With artifacts:** < 1 minute

## Troubleshooting

### "Release not found"

If Lake can't find the release:

```
error: failed to download https://github.com/yourusername/lean-uri/releases/download/v0.1.0/lean-uri-x86_64-unknown-linux-gnu.tar.gz
info: falling back to building from source
```

**Causes:**
1. The GitHub release `v0.1.0` doesn't exist
2. The archive wasn't uploaded to the release
3. The `rev` in lakefile.toml doesn't match any release tag

**Solution:**
- Lake automatically falls back to building from source
- Verify the release exists: `gh release view v0.1.0 --repo yourusername/lean-uri`
- Check available releases: `gh release list --repo yourusername/lean-uri`

### Wrong Platform

If you're on macOS but only Linux artifacts are available:

```
error: failed to download https://github.com/yourusername/lean-uri/releases/download/v0.1.0/lean-uri-aarch64-apple-darwin.tar.gz
info: falling back to building from source
```

**Solution:**
- The library maintainer needs to upload artifacts for your platform
- Or you build from source (which happens automatically)

### Corrupted Download

If the download is incomplete or corrupted:

```
error: failed to unpack lean-uri-x86_64-unknown-linux-gnu.tar.gz
info: falling back to building from source
```

**Solution:**
- Delete the package and update again: `rm -rf .lake/packages/lean-uri && lake update`
- Check your internet connection
- The maintainer may need to re-upload the archive

### Disabling Release Downloads

If you want to force building from source (e.g., for debugging):

```bash
# Temporary (this build only)
LAKE_NO_CACHE=1 lake build

# Or remove the setting from lakefile.toml
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "v0.1.0"
# preferReleaseBuild = false  # or omit entirely
```

## Advanced: Version Constraints

You can use different `rev` formats:

```toml
# Specific release tag (recommended for production)
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "v0.1.0"
preferReleaseBuild = true

# Specific commit hash (artifacts unlikely to exist)
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "a1b2c3d4e5f6"
preferReleaseBuild = true  # Will likely fall back to source build

# Branch name (artifacts unlikely to exist unless you tag every commit)
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "main"
preferReleaseBuild = true  # Will likely fall back to source build
```

**Best practice:** Use tagged releases (e.g., `v0.1.0`, `v1.0.0`) for `preferReleaseBuild`, since those are where artifacts are uploaded.

## Multiple Dependencies with Artifacts

```toml
name = "my-complex-app"

# Multiple dependencies, each with their own artifacts
[[require]]
name = "lean-uri"
git = "https://github.com/yourusername/lean-uri"
rev = "v0.1.0"
preferReleaseBuild = true

[[require]]
name = "some-proof-library"
git = "https://github.com/someorg/proof-library"
rev = "v2.3.0"
preferReleaseBuild = true

[[require]]
name = "mathlib4"
git = "https://github.com/leanprover-community/mathlib4"
rev = "v4.6.0"
preferReleaseBuild = true  # mathlib uses a cache system
```

Each dependency with `preferReleaseBuild = true` will download its own artifacts, dramatically reducing total build time.

## Testing Dependency Resolution

To see what Lake will do before running a full build:

```bash
# See all packages and their sources
lake print-paths

# See if releases would be fetched
lake update --verbose

# Force re-downloading everything
rm -rf .lake/packages
lake update
```

## Summary

**For downstream package maintainers:**
1. Add `preferReleaseBuild = true` to your dependency requirements
2. Use tagged releases as `rev` values
3. Run `lake update` to fetch artifacts
4. Enjoy fast builds!

**User experience benefits:**
- Faster onboarding (users can start coding immediately)
- Reproducible builds (everyone uses the same artifacts)
- Less dependency on build toolchain (curl + tar is enough to use the library)
- Reduced CI/CD build times
