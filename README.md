# LeanUri


![Build/Proof Check](https://github.com/josephmckinsey/lean-uri/actions/workflows/lean_action_ci.yml/badge.svg) ![Docs/Blueprint](https://github.com/josephmckinsey/lean-uri/actions/workflows/docs.yml/badge.svg)

[Docs](https://josephmckinsey.github.io/lean-uri/LeanUri/Basic.html)

Lean 4 library for RFC 3986 URIs: parse, print, normalize, and resolve.

- Parse absolute `URI` and relative version `RelativeRef` (no normalization)
- Serialize back to strings (round-trip friendly)
- Resolve relative references against a base `URI`
- Normalize URIs
- Equivalence via normalization
- Percent decoding with getters and `URI.encode` and `RelativeRef.encode`

Warning: Not efficient

## Quick start

```lean
import LeanUri.Basic
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

def u := URI.encode "https" (some "user name@host") "/foo bar" (some "q=1 2") (some "frag ment")
/-- info: "https://user%20name@host/foo%20bar?q=1%202#frag%20ment" -/
#guard_msgs in
#eval u.toString

def r := RelativeRef.parse "/foo/bar?x#y"
/--
info: Except.ok { authority := none, path := "/foo/bar", query := some "x", fragment := some "y" }
-/
#guard_msgs in
#eval r

/-- info: some "/dir/index.html" -/
#guard_msgs in
#eval base.getPath
```

## Installation (Lake)

Add LeanUri as a dependency to your `lakefile.toml`:

```toml
[[require]]
name = "lean-uri"
git = "https://github.com/josephmckinsey/lean-uri"
rev = "main"
```

Then import `LeanUri` for the high-level API, or `LeanUri.Defs` for types and no parsing.
