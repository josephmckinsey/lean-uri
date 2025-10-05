/-
IPv6 address and IP-literal parsing for URI (RFC 3986 Section 3.2.2)
-/

import LeanUri.Parsers
import LeanUri.IPv4
import Std.Internal.Parsec.String

namespace LeanUri

open Std.Internal.Parsec
open Std.Internal.Parsec.String

/-- h16 = 1*4HEXDIG (16 bits of address in hexadecimal) -/
def h16 : Parser String := do
  let first ← hexDigit
  let rest ← manyChars hexDigit
  let result := first.toString ++ rest
  if result.length > 4 then fail "h16 can have at most 4 hex digits"
  return result

/-- ls32 = ( h16 ":" h16 ) / IPv4address (least-significant 32 bits) -/
def ls32 : Parser String :=
  attempt (do
    let h1 ← h16
    skipChar ':'
    let h2 ← h16
    return h1 ++ ":" ++ h2)
  <|> ipv4address

/-- Helper to parse n repetitions of h16 followed by colon -/
def h16Colon : Parser String := do
  let h ← h16
  skipChar ':'
  return h ++ ":"

/-- Parse exactly n h16 groups with trailing colons -/
def nH16Colon (n : Nat) : Parser String := do
  let parts ← List.range n |>.mapM (fun _ => h16Colon)
  return String.join parts

/-- Parse optional prefix: up to n h16 groups with colons, then one h16 without colon -/
def optPrefix (maxGroups : Nat) : Parser String :=
  attempt (do
    let groups ← manyStrings (attempt (do
      let h ← h16
      skipChar ':'
      let next ← peek?
      if next == some ':' then fail "stop before ::"
      return h ++ ":"))
    let count := groups.toList.filter (· == ':') |>.length
    if count > maxGroups then fail s!"too many h16 groups (max {maxGroups})"
    let finalH16 ← h16
    return groups ++ finalH16)
  <|> pure ""

/-- IPv6address with all 9 variations from RFC 3986 -/
def ipv6address : Parser String :=
  -- Variation 1: 6( h16 ":" ) ls32
  attempt ((fun a b => a ++ b) <$> nH16Colon 6 <*> ls32)
  -- Variation 2: "::" 5( h16 ":" ) ls32
  <|> attempt ((fun p l => "::" ++ p ++ l) <$> (skipString "::" *> nH16Colon 5) <*> ls32)
  -- Variation 3: [ h16 ] "::" 4( h16 ":" ) ls32
  <|> attempt ((fun p s l => p ++ "::" ++ s ++ l) <$> (attempt h16 <|> pure "") <*> (skipString "::" *> nH16Colon 4) <*> ls32)
  -- Variation 4: [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
  <|> attempt ((fun p s l => p ++ "::" ++ s ++ l) <$> optPrefix 1 <*> (skipString "::" *> nH16Colon 3) <*> ls32)
  -- Variation 5: [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
  <|> attempt ((fun p s l => p ++ "::" ++ s ++ l) <$> optPrefix 2 <*> (skipString "::" *> nH16Colon 2) <*> ls32)
  -- Variation 6: [ *3( h16 ":" ) h16 ] "::" h16 ":" ls32
  <|> attempt ((fun p s l => p ++ "::" ++ s ++ l) <$> optPrefix 3 <*> (skipString "::" *> h16Colon) <*> ls32)
  -- Variation 7: [ *4( h16 ":" ) h16 ] "::" ls32
  <|> attempt ((fun p l => p ++ "::" ++ l) <$> optPrefix 4 <*> (skipString "::" *> ls32))
  -- Variation 8: [ *5( h16 ":" ) h16 ] "::" h16
  <|> attempt ((fun p h => p ++ "::" ++ h) <$> optPrefix 5 <*> (skipString "::" *> h16))
  -- Variation 9: [ *6( h16 ":" ) h16 ] "::"
  <|> ((fun p => p ++ "::") <$> optPrefix 6 <* skipString "::")

def ipvFuture : Parser String := do
  skipChar 'v'
  let version ← manyChars hexDigit
  if version.isEmpty then fail "IPvFuture requires at least one hex digit"
  skipChar '.'
  let first ← unreserved <|> subDelims <|> satisfy (· == ':')
  let rest ← manyChars (unreserved <|> subDelims <|> satisfy (· == ':'))
  return "v" ++ version ++ "." ++ first.toString ++ rest

/-- IP-literal = "[" ( IPv6address / IPvFuture ) "]" -/
def ipLiteral : Parser String := do
  skipChar '['
  let addr ← attempt ipv6address <|> attempt ipvFuture
  skipChar ']'
  return "[" ++ addr ++ "]"

end LeanUri
