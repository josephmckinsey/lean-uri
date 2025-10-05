/-
Copyright (c) 2025 Joseph
Released under Apache 2.0 license
Implementation of RFC 3986 - URI Generic Syntax
-/

import LeanUri.Parsers
import LeanUri.IPv4
import LeanUri.IPv6
import LeanUri.URI
import LeanUri.Normalization

namespace LeanUri

def URI.ofString (s : String) : Except String URI :=
  Internal.uri.run s

def URI.resolve (baseUri : URI) (reference : String) : Except String URI :=
  match Internal.uriReference.run reference with
  | .ok (Sum.inl absoluteUri) => .ok absoluteUri
  | .ok (Sum.inr relRef) => .ok (Internal.resolve baseUri relRef)
  | .error e => .error e

def URI.normalize : URI → URI := Internal.normalizeAll

end LeanUri
