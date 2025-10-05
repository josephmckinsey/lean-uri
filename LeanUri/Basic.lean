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

/-! ## IPv6 Address (RFC 3986 Section 3.2.2) -/

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

/-- host = IP-literal / IPv4address / reg-name -/
def host : Parser String :=
  attempt ipLiteral <|> attempt ipv4address <|> regName

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

/-! ## Complete URI (RFC 3986 Section 3) -/

/-- Parsed URI components -/
structure URI where
  scheme : String
  authority : Option String
  path : String
  query : Option String
  fragment : Option String
deriving Repr, BEq

instance : ToString URI where
  toString uri :=
    uri.scheme ++ ":" ++
    (match uri.authority with
    | some auth => "//" ++ auth
    | none => "") ++
    uri.path ++
    (match uri.query with
    | some q => "?" ++ q
    | none => "") ++
    (match uri.fragment with
    | some f => "#" ++ f
    | none => "")

/-- hier-part = "//" authority path-abempty
              / path-absolute
              / path-rootless
              / path-empty -/
def hierPart : Parser (Option String × String) :=
  -- Try "//" authority path-abempty
  (attempt do
    skipString "//"
    let auth ← authority
    let pathPart ← pathAbempty
    return (some auth, pathPart)
  )
  -- Or try path-absolute
  <|> (attempt do
    let pathPart ← pathAbsolute
    return (none, pathPart)
  )
  -- Or try path-rootless
  <|> (attempt do
    let pathPart ← pathRootless
    return (none, pathPart)
  )
  -- Or path-empty
  <|> do
    let pathPart ← pathEmpty
    return (none, pathPart)

/-- URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ] -/
def uri : Parser URI := do
  let schemePart ← scheme
  skipChar ':'
  let (auth, pathPart) ← hierPart
  let queryPart ← (attempt (skipChar '?' *> query >>= fun q => pure (some q))) <|> pure none
  let fragPart ← (attempt (skipChar '#' *> fragment >>= fun f => pure (some f))) <|> pure none

  return {
    scheme := schemePart
    authority := auth
    path := pathPart
    query := queryPart
    fragment := fragPart
  }

/-! ## Relative References (RFC 3986 Section 4.2) -/

/-- A relative reference (without scheme) -/
structure RelativeRef where
  authority : Option String
  path : String
  query : Option String
  fragment : Option String
  deriving Repr

/-- relative-part = "//" authority path-abempty / path-absolute / path-noscheme / path-empty -/
def relativePart : Parser (Option String × String) :=
  (do
    skipString "//"
    let auth ← authority
    let p ← pathAbempty
    return (some auth, p))
  <|> (do
    let p ← pathAbsolute
    return (none, p))
  <|> (do
    let p ← pathNoscheme
    return (none, p))
  <|> (do
    let p ← pathEmpty
    return (none, p))

/-- relative-ref = relative-part [ "?" query ] [ "#" fragment ] -/
def relativeRef : Parser RelativeRef := do
  let (auth, pathPart) ← relativePart
  let queryPart ← (attempt (skipChar '?' *> query >>= fun q => pure (some q))) <|> pure none
  let fragPart ← (attempt (skipChar '#' *> fragment >>= fun f => pure (some f))) <|> pure none

  return {
    authority := auth
    path := pathPart
    query := queryPart
    fragment := fragPart
  }

/-! ## Reference Resolution (RFC 3986 Section 5.2) -/

/-- Remove dot segments from a path (RFC 3986 Section 5.2.4) -/
partial def removeDotSegments (input : String) (output : String := "") : String :=
  if input.isEmpty then
    output
  else if input.startsWith "../" then
    removeDotSegments (input.drop 3) output
  else if input.startsWith "./" then
    removeDotSegments (input.drop 2) output
  else if input.startsWith "/./" then
    removeDotSegments ("/" ++ input.drop 3) output
  else if input == "/." then
    removeDotSegments "/" output
  else if input.startsWith "/../" then
    -- Remove last segment from output
    let lastSlash := output.dropRightWhile (· != '/')
    let newOutput := if lastSlash.isEmpty then "" else lastSlash.dropRight 1
    removeDotSegments ("/" ++ input.drop 4) newOutput
  else if input == "/.." then
    -- Remove last segment from output
    let lastSlash := output.dropRightWhile (· != '/')
    let newOutput := if lastSlash.isEmpty then "" else lastSlash.dropRight 1
    removeDotSegments "/" newOutput
  else if input == "." || input == ".." then
    ""
  else
    -- Move the first path segment to output
    let afterFirst := input.drop 1
    let charsUntilSlash := afterFirst.takeWhile (· != '/')
    let segmentEnd := charsUntilSlash.length + 1
    let segment := input.take segmentEnd
    removeDotSegments (input.drop segmentEnd) (output ++ segment)

/-- Merge a base path with a reference path (RFC 3986 Section 5.2.3) -/
def mergePaths (basePath : String) (refPath : String) (baseHasAuth : Bool) : String :=
  if baseHasAuth && basePath.isEmpty then
    "/" ++ refPath
  else
    let baseDir := basePath.dropRightWhile (· != '/')
    baseDir ++ refPath

/-- Resolve a relative reference against a base URI (RFC 3986 Section 5.2.2) -/
def resolve (base : URI) (ref : RelativeRef) : URI :=
  let targetScheme := base.scheme
  let (targetAuth, targetPath, targetQuery) :=
    match ref.authority with
    | some auth =>
      (some auth, removeDotSegments ref.path, ref.query)
    | none =>
      if ref.path.isEmpty then
        (base.authority, base.path, ref.query.or base.query)
      else if ref.path.startsWith "/" then
        (base.authority, removeDotSegments ref.path, ref.query)
      else
        let merged := mergePaths base.path ref.path (base.authority.isSome)
        (base.authority, removeDotSegments merged, ref.query)

  {
    scheme := targetScheme
    authority := targetAuth
    path := targetPath
    query := targetQuery
    fragment := ref.fragment
  }

/-- URI-reference = URI / relative-ref -/
def uriReference : Parser (URI ⊕ RelativeRef) :=
  (attempt (uri >>= fun u => pure (Sum.inl u)))
  <|> (relativeRef >>= fun r => pure (Sum.inr r))

/-- Parse and resolve a URI reference against a base URI -/
def parseAndResolve (baseUri : URI) (reference : String) : Except String URI :=
  match uriReference.run reference with
  | .ok (Sum.inl absoluteUri) => .ok absoluteUri
  | .ok (Sum.inr relRef) => .ok (resolve baseUri relRef)
  | .error e => .error e

end LeanUri
