/-
Parsers and character predicates for URI parsing (RFC 3986)
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

end LeanUri
