/-!
# Core data types for RFC 3986 URIs

This module defines the `URI` (absolute URI) and `RelativeRef` (relative reference)
records, plus basic conversions and printer instances. Strings in fields are stored
without their leading delimiters (no leading `?` for `query`, no `#` for `fragment`,
no `//` for `authority`).

The types store components exactly as parsed; normalization is performed elsewhere.
-/

namespace LeanUri

/-- An absolute Uniform Resource Identifier as defined by RFC 3986.

All components are stored as they appear (no normalization is performed here). -/
structure URI where
  -- The scheme, e.g., `http`, `https`, `mailto`. Case is preserved as provided.
  scheme : String
  -- Optional authority component (userinfo, host, and port), without the leading `//`.
  authority : Option String := none
  -- The path component (may be empty). Stored exactly as parsed.
  path : String
  -- Optional query component without the leading `?`.
  query : Option String := none
  -- Optional fragment component without the leading `#`.
  fragment : Option String := none
deriving Repr, BEq, Hashable

instance : Inhabited URI where
  default := { scheme := "data", path := "" }

/-- A relative reference per RFC 3986 (i.e., no scheme).

This type can still include an authority, path, query, and fragment. -/
structure RelativeRef where
  -- Optional authority component without the leading `//`.
  authority : Option String
  -- The path component (may be empty). Stored exactly as parsed.
  path : String
  -- Optional query component without the leading `?`.
  query : Option String
  -- Optional fragment component without the leading `#`.
  fragment : Option String
deriving Inhabited, Repr, BEq, Hashable

/-- Drop the scheme of a `URI` and convert it to a `RelativeRef`,
keeping all other components unchanged. -/
def URI.toRelativeRef (uri : URI) : RelativeRef := {
  authority := uri.authority
  path := uri.path
  query := uri.query
  fragment := uri.fragment
}

/-- Serialize a `RelativeRef` to its RFC 3986 string form.

Includes `//authority` when present, appends `?query` and `#fragment` when present,
and does not apply any normalization. -/
instance : ToString RelativeRef where
  toString ref :=
    (match ref.authority with
    | some auth => "//" ++ auth
    | none => "") ++
    ref.path ++
    (match ref.query with
    | some q => "?" ++ q
    | none => "") ++
    (match ref.fragment with
    | some f => "#" ++ f
    | none => "")

/-- Serialize a `URI` to `scheme ":"` followed by the serialization of its relative part.
The output is not normalized. -/
instance : ToString URI where
  toString uri := uri.scheme ++ ":" ++ toString uri.toRelativeRef

end LeanUri
