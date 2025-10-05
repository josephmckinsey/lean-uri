# LeanUri

Lean 4 library for RFC 3986 URIs: parse, print, normalize, and resolve.

- Parse absolute `URI` and relative version `RelativeRef` (no normalization)
- Serialize back to strings (round-trip friendly)
- Resolve relative references against a base `URI`
- Normalize URIs
- Equivalence via normalization

Warning: Not efficient

## Quick start

```lean
import LeanUri
open LeanUri

/-- info: "https://example.com/a/c?x#y" -/
#guard_msgs in
#eval match URI.parse "https://EXAMPLE.com/a/./b/../c?x#y" with
| .ok u   => (URI.normalize u).toString
| .error e => s!"error: {e}"

def base : URI := { scheme := "https", authority := some "example.com", path := "/dir/index.html" }
/-- info: Except.ok "https://example.com/x" -/
#guard_msgs in
#eval (URI.resolve base "../x").map (·.toString)
```

## Installation (Lake)

Add LeanUri as a dependency to your `lakefile.toml`:

```toml
[[require]]
scope = "josephmckinsey"
name = "lean-uri"
rev = "main"
```

Then import `LeanUri` for the high-level API, or `LeanUri.Defs` for types and no parsing.
