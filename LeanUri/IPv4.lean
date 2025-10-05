/-
IPv4 address parsing for URI (RFC 3986 Section 3.2.2)
-/

import LeanUri.Parsers
import Std.Internal.Parsec.String

namespace LeanUri

open Std.Internal.Parsec
open Std.Internal.Parsec.String

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

end LeanUri
