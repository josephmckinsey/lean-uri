import LeanUri.Parsers
import LeanUri.IPv4
import LeanUri.IPv6
import LeanUri.URI
import LeanUri.Normalization

/-!
# High-level API for RFC 3986 URIs

This module exposes a small, convenient surface over the internal parsers,
normalizers, and resolvers:

- parsing helpers for absolute `URI`s and `RelativeRef`s
- pretty-printers (string serialization) for `URI` and `RelativeRef`
- normalization and equivalence checks
- resolution of relative references against a base `URI`

It delegates to the `Internal` parser/normalizer/resolver implemented in
`LeanUri.Parsers`, `LeanUri.Normalization`, and related modules.
-/

namespace LeanUri

/-- Parse an absolute `URI` from a string without applying normalization.

Returns `.ok uri` on success or `.error msg` if parsing fails.

## Examples
```lean
open LeanUri
match URI.parse "https://example.com/a?b#c" with
| .ok u   => (u.scheme, u.authority.isSome, u.path) = ("https", true, "/a")
| .error _ => False
```
-/
@[inline]
def URI.parse : String → Except String URI := Internal.uri.run

/-- Parse a `RelativeRef` (relative reference) from a string without normalization.

Returns `.ok ref` on success or `.error msg` on parse failure.

## Examples
```lean
open LeanUri
match RelativeRef.parse "../x?y#z" with
| .ok r   => (r.authority, r.path, r.query, r.fragment) = (none, "../x", some "y", some "z")
| .error _ => False
```
-/
@[inline]
def RelativeRef.parse : String → Except String RelativeRef := Internal.relativeRef.run

/-- Parse either an absolute `URI` or a `RelativeRef` from a string.

Returns `Sum.inl uri` for an absolute URI, or `Sum.inr ref` for a relative reference.

## Examples
```lean
open LeanUri
( parseReference "mailto:user@example.com" ).isOk -- absolute → inl
( parseReference "./a/b" ).isOk                  -- relative → inr
```
-/
@[inline]
def parseReference : String → Except String (Sum URI RelativeRef) :=
  Internal.uriReference.run

/-- Serialize a `URI` to a string without applying any normalization.

This is a convenience wrapper around the `ToString` instance.

## Examples
```lean
open LeanUri
let u : URI := { scheme := "https", authority := some "example.com", path := "/a", query := some "q=1", fragment := none }
URI.toString u = "https://example.com/a?q=1"
```
-/
@[inline]
def URI.toString : URI → String := ToString.toString

/-- Serialize a `RelativeRef` to a string without applying any normalization.

This is a convenience wrapper around the `ToString` instance.

## Examples
```lean
open LeanUri
let r : RelativeRef := { authority := some "example.com", path := "/p", query := none, fragment := some "frag" }
RelativeRef.toString r = "//example.com/p#frag"
```
-/
@[inline]
def RelativeRef.toString : RelativeRef → String := ToString.toString

/-- Normalize all components of a `URI` according to the RFC 3986 rules
implemented by this library (e.g., case normalization where applicable, removal
of dot segments, etc.).

## Examples
```lean
open LeanUri
let u : URI := { scheme := "http", authority := some "EXAMPLE.com", path := "/a/./b/../c", query := none, fragment := none }
URI.normalize u |>.toString = "http://example.com/a/c"
```
-/
@[inline]
def URI.normalize : URI → URI := Internal.normalizeAll

/-- Resolve a string reference against a base `URI`.

- If `reference` is an absolute URI, it is returned as-is.
- If `reference` is a relative reference, it is resolved against `baseUri` using `Internal.resolve`.
- Returns `.error msg` when `reference` cannot be parsed as a URI or relative reference.

## Examples
```lean
open LeanUri
let base : URI := { scheme := "https", authority := some "example.com", path := "/dir/index.html", query := none, fragment := none }
URI.resolve base "../x" = .ok { scheme := "https", authority := some "example.com", path := "/x", query := none, fragment := none }
```
-/
def URI.resolve (baseUri : URI) (reference : String) : Except String URI :=
  match Internal.uriReference.run reference with
  | .ok (Sum.inl absoluteUri) => .ok absoluteUri
  | .ok (Sum.inr relRef) => .ok (Internal.resolve baseUri relRef)
  | .error e => .error e

/-- Resolve a pre-parsed `RelativeRef` against a base `URI`.

## Examples
```lean
open LeanUri
let base : URI := { scheme := "https", authority := some "example.com", path := "/a/b/", query := none, fragment := none }
let rr   : RelativeRef := { authority := none, path := "c/../d", query := none, fragment := none }
URI.resolveRef base rr |>.toString = "https://example.com/a/d"
```
-/
@[inline]
def URI.resolveRef : URI → RelativeRef → URI := Internal.resolve

/-- Return `true` when the URI has an authority component.

Note: This checks for the presence of an authority (i.e., `//…`), which is not
the same as checking for the presence of a scheme.

## Examples
```lean
open LeanUri
let a : URI := { scheme := "http",  authority := some "example.com", path := "/", query := none, fragment := none }
let b : URI := { scheme := "mailto", authority := none,               path := "user@example.com", query := none, fragment := none }
(URI.isAbsolute a, URI.isAbsolute b) = (true, false)
```
-/
@[inline]
def URI.isAbsolute (uri : URI) : Bool := uri.authority.isSome

/-- Check whether two URIs are equivalent after normalization.

## Examples
```lean
open LeanUri
let u1 : URI := { scheme := "http", authority := some "EXAMPLE.com", path := "/a/./b/../c", query := some "Q=1", fragment := none }
let u2 : URI := { scheme := "http", authority := some "example.COM", path := "/a/c",         query := some "q=1", fragment := none }
URI.equivalent u1 u2 = true
```
-/
@[inline]
def URI.equivalent (uri1 : URI) (uri2 : URI) : Bool :=
  uri1.normalize == uri2.normalize

open Internal in
/-- Construct a URI from raw (unencoded) components, automatically percent-encoding as required by RFC 3986.
    This ensures the resulting URI is valid and safe for serialization. -/
@[inline]
def URI.mkEncoded (scheme : String)
    (authority : Option String := none)
    (path : String := "")
    (query : Option String := none)
    (fragment : Option String := none)
  : URI :=
  let encodeAuth := authority.map (pctEncode (fun c => isUnreserved c || isSubDelim c || c == ':' || c == '@'))
  let encodePath := pctEncode isPathChar path
  let encodeQuery := query.map (pctEncode isQueryOrFragmentChar)
  let encodeFragment := fragment.map (pctEncode isQueryOrFragmentChar)
  {
    scheme := scheme,
    authority := encodeAuth,
    path := encodePath,
    query := encodeQuery,
    fragment := encodeFragment
  }

open Internal in
/-- Construct a RelativeRef from raw (unencoded) components, automatically percent-encoding as required by RFC 3986.
    This ensures the resulting RelativeRef is valid and safe for serialization. -/
@[inline]
def RelativeRef.mkEncoded (
    authority : Option String := none)
    (path : String := "")
    (query : Option String := none)
    (fragment : Option String := none)
  : RelativeRef :=
  let encodeAuth := authority.map (pctEncode (fun c => isUnreserved c || isSubDelim c || c == ':' || c == '@'))
  let encodePath := pctEncode isPathChar path
  let encodeQuery := query.map (pctEncode isQueryOrFragmentChar)
  let encodeFragment := fragment.map (pctEncode isQueryOrFragmentChar)
  {
    authority := encodeAuth,
    path := encodePath,
    query := encodeQuery,
    fragment := encodeFragment
  }

/-- Get the decoded (unescaped) authority component of a RelativeRef, if present. -/
@[inline]
def RelativeRef.getAuthority (r : RelativeRef) : Option String :=
  r.authority >>= (fun s => match pctDecode s with | .ok r => some r | .error _ => none)

/-- Get the decoded (unescaped) path component of a RelativeRef. -/
@[inline]
def RelativeRef.getPath (r : RelativeRef) : Option String :=
  match pctDecode r.path with | .ok r => some r | .error _ => none

/-- Get the decoded (unescaped) query component of a RelativeRef, if present. -/
@[inline]
def RelativeRef.getQuery (r : RelativeRef) : Option String :=
  r.query >>= (fun s => match pctDecode s with | .ok r => some r | .error _ => none)

/-- Get the decoded (unescaped) fragment component of a RelativeRef, if present. -/
@[inline]
def RelativeRef.getFragment (r : RelativeRef) : Option String :=
  r.fragment >>= (fun s => match pctDecode s with | .ok r => some r | .error _ => none)

/-- Get the decoded (unescaped) authority component of a URI, if present. -/
@[inline]
def URI.getAuthority (u : URI) : Option String :=
  u.authority >>= (fun s => match pctDecode s with | .ok r => some r | .error _ => none)

/-- Get the decoded (unescaped) path component of a URI. -/
@[inline]
def URI.getPath (u : URI) : Option String :=
  match pctDecode u.path with | .ok r => some r | .error _ => none

/-- Get the decoded (unescaped) query component of a URI, if present. -/
@[inline]
def URI.getQuery (u : URI) : Option String :=
  u.query >>= (fun s => match pctDecode s with | .ok r => some r | .error _ => none)

/-- Get the decoded (unescaped) fragment component of a URI, if present. -/
@[inline]
def URI.getFragment (u : URI) : Option String :=
  u.fragment >>= (fun s => match pctDecode s with | .ok r => some r | .error _ => none)

end LeanUri
