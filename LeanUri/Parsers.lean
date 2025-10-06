/-
Parsers and character predicates for URI parsing (RFC 3986)
-/

import Std.Internal.Parsec.String

namespace LeanUri.Internal

open Std.Internal
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

/-! ## Component-specific allowed character predicates (RFC 3986) -/

/-- Allowed in a path segment: unreserved, sub-delims, ':', '@' --/
def isPathChar (c : Char) : Bool :=
  isUnreserved c || isSubDelim c || c == ':' || c == '@'

/-- Allowed in a query or fragment: unreserved, sub-delims, ':', '@', '/', '?' --/
def isQueryOrFragmentChar (c : Char) : Bool :=
  isUnreserved c || isSubDelim c || c == ':' || c == '@' || c == '/' || c == '?'

/-- Allowed in userinfo: unreserved, sub-delims, ':' --/
def isUserinfoChar (c : Char) : Bool :=
  isUnreserved c || isSubDelim c || c == ':'

/-- Allowed in reg-name (host): unreserved, sub-delims --/
def isRegNameChar (c : Char) : Bool :=
  isUnreserved c || isSubDelim c

/-- Allowed in port: digit --/
def isPortChar (c : Char) : Bool :=
  '0' ≤ c && c ≤ '9'

/-! ## Percent Decoding Chars -/

/-- Convert a hexadecimal character to its numeric value.
    Assumes input is a valid HEXDIG (produced by the parser). -/
def hexCharToUInt8 (c : Char) : UInt8 :=
  if '0' ≤ c && c ≤ '9' then
    c.toUInt8 - '0'.toUInt8
  else if 'A' ≤ c && c ≤ 'F' then
    c.toUInt8 - 'A'.toUInt8 + 10
  else
    -- assume lowercase hex
    c.toUInt8 - 'a'.toUInt8 + 10

def uInt8toOneHexChar (x : UInt8) : Char :=
  if x < 10 then
    Char.ofUInt8 (x + '0'.toUInt8)
  else if x < 16 then
    Char.ofUInt8 (x + 'A'.toUInt8)
  else
    Char.ofUInt8 0

/-- Convert a UInt8 to percent encoded-/
def uInt8ToHexChars (c : UInt8) : String :=
  let h1 := uInt8toOneHexChar (c % 16)
  let h2 := uInt8toOneHexChar (c / 16)
  s!"%{h1}{h2}"

def decodeCharsToUInt8 (h1 h2 : Char) : UInt8 :=
  let v1 := hexCharToUInt8 h1
  let v2 := hexCharToUInt8 h2
  (v1 * 16 + v2)

def parseOnePctNormalize (allowed : Char → Bool) : Parser String := do
  skipChar '%'
  let h1 ← hexDigit
  let h2 ← hexDigit
  let char := Char.ofUInt8 <| decodeCharsToUInt8 h1 h2
  if allowed char then
    return char.toString
  return s!"%{h1}{h2}"

def pctDecodeOne : Parser UInt8 := do
  skipChar '%'
  let h1 ← hexDigit
  let h2 ← hexDigit
  return decodeCharsToUInt8 h1 h2

section appendUtils

variable {α : Type} {ι : Type} {elem : Type} {idx : Type}
variable [DecidableEq idx] [DecidableEq elem] [Input ι elem idx]
variable [Append α]

@[specialize]
partial def manyAppendCore (p : Parsec ι α) (acc : α) : Parsec ι α :=
  tryCatch p (manyAppendCore p <| acc ++ ·) (fun _ => pure acc)

@[inline]
def manyAppendArray (p : Parsec ι (Array β)) : Parsec ι (Array β) := manyAppendCore p #[]

@[inline]
def manyAppendList (p : Parsec ι (List β)) : Parsec ι (List β) := manyAppendCore p []

@[inline]
def manyAppendString (p : Parsec ι String) : Parsec ι String := manyAppendCore p ""

end appendUtils

def pctDecodeToByteString : Parser ByteArray := do
  ByteArray.mk <$> manyAppendArray (
    (fun x => #[x]) <$> pctDecodeOne <|>
    (fun x => x.toString.toUTF8.data) <$> any
  )

def pctEncodeChar (c : Char) : String :=
  String.join (c.toString.toUTF8.data.map uInt8ToHexChars).toList

def parsePctNormalize (allowed : Char → Bool) : Parser String :=
  manyAppendString (parseOnePctNormalize allowed <|> toString <$> any)

def pctNormalize (allowed : Char → Bool) (s : String) : String :=
  match (parsePctNormalize allowed).run s with
  | .ok result => result
  | .error _ => s

end Internal

/-- Decode percent encoded string into ordinary string.
-/
def pctDecode (s : String) : Except String String :=
  match Internal.pctDecodeToByteString.run s with
  | .ok byteString => match String.fromUTF8? byteString with
    | .some s => .ok s
    | .none => .error "Could not decode UTF8 string"
  | .error msg => .error msg

-- I gave up and wrote this one iteratively. This can probaly be a parser run
/-- Encode all allowed chars in a string to percent encoded bytes
-/
def pctEncode (allowed : Char → Bool) (s : String) : String := Id.run do
  let mut acc := ""
  let mut charIter := s.iter
  while h : charIter.hasNext do
    let c := charIter.curr' h
    if allowed c then
      acc := acc.push c
    else
      acc := acc.append (Internal.pctEncodeChar c)
  return acc

end LeanUri
