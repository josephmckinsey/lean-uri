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

/-- Parse zero or more with a parser and accumulate the results into a string -/
partial def manyStrings (p : Parser String) (acc : String := "") : Parser String :=
  tryCatch p (fun s => manyStrings p (acc ++ s)) (fun _ => pure acc)

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
def userinfo : Parser String :=
  manyStrings (
    pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
    <|> (·.toString) <$> pchar ':'
  )

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
def regName : Parser String :=
  manyStrings (
    pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
  )

/-! ## Port (RFC 3986 Section 3.2.3) -/

/-- port = *DIGIT -/
def port : Parser String := manyChars digit

/-! ## Host (RFC 3986 Section 3.2.2) -/

/-- host = IP-literal / IPv4address / reg-name
    Note: We don't support IPv6 yet, so IP-literal is omitted -/
def host : Parser String :=
  attempt ipv4address <|> regName

/-! ## Authority (RFC 3986 Section 3.2) -/

/-- authority = [ userinfo "@" ] host [ ":" port ] -/
def authority : Parser String := do
  let userInfo ← (attempt (userinfo <* pchar '@')) <|> pure ""
  let hostPart ← host
  let portPart ← (attempt (pchar ':' *> port)) <|> pure ""

  return (if userInfo.isEmpty then "" else userInfo ++ "@") ++
    hostPart ++
    (if portPart.isEmpty then "" else ":" ++ portPart)

/-! ## Path (RFC 3986 Section 3.3) -/

/-- pchar = unreserved / pct-encoded / sub-delims / ":" / "@" -/
def pchar : Parser String :=
  pctEncoded
  <|> (·.toString) <$> unreserved
  <|> (·.toString) <$> subDelims
  <|> (·.toString) <$> satisfy (· == ':')
  <|> (·.toString) <$> satisfy (· == '@')

/-- segment = *pchar -/
def segment : Parser String :=
  manyStrings pchar

/-- segment-nz = 1*pchar (non-zero-length segment) -/
def segmentNz : Parser String := do
  let first ← pchar
  let rest ← manyStrings pchar
  return first ++ rest

/-- segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
    (non-zero-length segment without any colon ":") -/
def segmentNzNc : Parser String := do
  let first ← pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
    <|> (·.toString) <$> satisfy (· == '@')
  let rest ← manyStrings (
    pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
    <|> (·.toString) <$> satisfy (· == '@')
  )
  return first ++ rest

/-- path-abempty = *( "/" segment ) -/
def pathAbempty : Parser String :=
  manyStrings (pstring "/" *> segment >>= fun s => pure ("/" ++ s))

/-- path-absolute = "/" [ segment-nz *( "/" segment ) ] -/
def pathAbsolute : Parser String := do
  skipChar '/'
  let rest ← (attempt do
    let first ← segmentNz
    let segs ← manyStrings (pstring "/" *> segment >>= fun s => pure ("/" ++ s))
    return first ++ segs
  ) <|> pure ""
  return "/" ++ rest

/-- path-noscheme = segment-nz-nc *( "/" segment ) -/
def pathNoscheme : Parser String := do
  let first ← segmentNzNc
  let rest ← manyStrings (pstring "/" *> segment >>= fun s => pure ("/" ++ s))
  return first ++ rest

/-- path-rootless = segment-nz *( "/" segment ) -/
def pathRootless : Parser String := do
  let first ← segmentNz
  let rest ← manyStrings (pstring "/" *> segment >>= fun s => pure ("/" ++ s))
  return first ++ rest

/-- path-empty = 0<pchar> -/
def pathEmpty : Parser String :=
  pure ""

/-! ## Query (RFC 3986 Section 3.4) -/

/-- query = *( pchar / "/" / "?" ) -/
def query : Parser String :=
  manyStrings (
    pchar
    <|> (·.toString) <$> satisfy (· == '/')
    <|> (·.toString) <$> satisfy (· == '?')
  )

/-! ## Fragment (RFC 3986 Section 3.5) -/

/-- fragment = *( pchar / "/" / "?" ) -/
def fragment : Parser String :=
  manyStrings (
    pchar
    <|> (·.toString) <$> satisfy (· == '/')
    <|> (·.toString) <$> satisfy (· == '?')
  )

end LeanUri
