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

def URI.resolve (baseUri : URI) (reference : String) : Except String URI :=
  match Internal.uriReference.run reference with
  | .ok (Sum.inl absoluteUri) => .ok absoluteUri
  | .ok (Sum.inr relRef) => .ok (Internal.resolve baseUri relRef)
  | .error e => .error e

@[inline]
def URI.normalize : URI → URI := Internal.normalizeAll

@[inline]
def URI.parse : String → Except String URI := Internal.uri.run

@[inline]
def RelativeRef.parse : String → Except String RelativeRef := Internal.relativeRef.run

@[inline]
def parseReference : String → Except String (Sum URI RelativeRef) :=
  Internal.uriReference.run

@[inline]
def URI.toString : URI → String := ToString.toString

@[inline]
def RelativeRef.toString : RelativeRef → String := ToString.toString

def URI.equivalent (uri1 : URI) (uri2 : URI) : Bool :=
  uri1.normalize == uri2.normalize

def URI.resolveRef : URI → RelativeRef → URI := Internal.resolve

def URI.isAbsolute (uri : URI) : Bool := uri.authority.isSome

end LeanUri
