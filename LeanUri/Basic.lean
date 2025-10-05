/-
Copyright (c) 2025 Joseph
Released under Apache 2.0 license
Implementation of RFC 3986 - URI Generic Syntax
-/

import Std.Internal.Parsec.String

namespace LeanUri

open Std.Internal.Parsec
open Std.Internal.Parsec.String

/-! ## Character Predicates (RFC 3986 Section 2) -/

/-- ALPHA = %x41-5A / %x61-7A (A-Z / a-z) -/
def isAlpha (c : Char) : Bool :=
  ('A' ≤ c && c ≤ 'Z') || ('a' ≤ c && c ≤ 'z')

/-- HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F" -/
def isHexDigit (c : Char) : Bool :=
  ('0' ≤ c && c ≤ '9') || ('A' ≤ c && c ≤ 'F') || ('a' ≤ c && c ≤ 'f')

/-- unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" -/
def isUnreserved (c : Char) : Bool :=
  isAlpha c || ('0' ≤ c && c ≤ '9') || c == '-' || c == '.' || c == '_' || c == '~'

/-- gen-delims = ":" / "/" / "?" / "#" / "[" / "]" / "@" -/
def isGenDelim (c : Char) : Bool :=
  c == ':' || c == '/' || c == '?' || c == '#' || c == '[' || c == ']' || c == '@'

/-- sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "=" -/
def isSubDelim (c : Char) : Bool :=
  c == '!' || c == '$' || c == '&' || c == '\'' || c == '(' || c == ')' ||
  c == '*' || c == '+' || c == ',' || c == ';' || c == '='

/-- reserved = gen-delims / sub-delims -/
def isReserved (c : Char) : Bool :=
  isGenDelim c || isSubDelim c

/-! ## Basic Parsers (RFC 3986 Section 2) -/

/-- Parse a percent-encoded octet: pct-encoded = "%" HEXDIG HEXDIG -/
def pctEncoded : Parser String := do
  skipChar '%'
  let h1 ← hexDigit
  let h2 ← hexDigit
  return s!"%{h1}{h2}"

/-- unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" -/
def unreserved : Parser Char := satisfy isUnreserved

/-- sub-delims = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "=" -/
def subDelims : Parser Char := satisfy isSubDelim

/-! ## Scheme (RFC 3986 Section 3.1) -/

/-- scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) -/
def scheme : Parser String := do
  let first ← satisfy isAlpha
  let rest ← manyChars (satisfy fun c => isAlpha c || ('0' ≤ c && c ≤ '9') || c == '+' || c == '-' || c == '.')
  return first.toString ++ rest

/-! ## User Info (RFC 3986 Section 3.2.1) -/

/-- userinfo = *( unreserved / pct-encoded / sub-delims / ":" ) -/
def userinfo : Parser String := do
  let parts ← many (
    pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
    <|> (·.toString) <$> pchar ':'
  )
  return String.join parts.toList

/-! ## IPv4 Address (RFC 3986 Section 3.2.2) -/

/-- dec-octet = DIGIT / %x31-39 DIGIT / "1" 2DIGIT / "2" %x30-34 DIGIT / "25" %x30-35 -/
def decOctet : Parser Nat := do
  let n ← digits
  if n > 255 then
    fail s!"decimal octet out of range: {n}"
  else
    return n

/-- IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet -/
def ipv4address : Parser String := do
  let o1 ← decOctet
  skipChar '.'
  let o2 ← decOctet
  skipChar '.'
  let o3 ← decOctet
  skipChar '.'
  let o4 ← decOctet
  return s!"{o1}.{o2}.{o3}.{o4}"

/-! ## Registered Name (RFC 3986 Section 3.2.2) -/

/-- reg-name = *( unreserved / pct-encoded / sub-delims ) -/
def regName : Parser String := do
  let parts ← many (
    pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
  )
  return String.join parts.toList

end LeanUri
