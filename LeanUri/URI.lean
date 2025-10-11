/-
Main URI structure, authority, path, and resolution logic for RFC 3986
-/

import LeanUri.Parsers
import LeanUri.IPv4
import LeanUri.IPv6
import LeanUri.Defs
import Std.Internal.Parsec.String

namespace LeanUri.Internal

open Std.Internal.Parsec
open Std.Internal.Parsec.String

/-- userinfo = *( unreserved / pct-encoded / sub-delims / ":" ) -/
def userinfo : Parser String :=
  manyStrings (
    pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
    <|> (·.toString) <$> pchar ':'
  )

/-- port = *DIGIT -/
def port : Parser String := manyChars digit

/-- host = IP-literal / IPv4address / reg-name -/
def regName : Parser String :=
  manyStrings (
    pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
  )

def host : Parser String :=
  attempt ipLiteral <|> attempt ipv4address <|> regName

/-- authority = [ userinfo "@" ] host [ ":" port ] -/
def authority : Parser String := do
  let userInfo ← (attempt (userinfo <* pchar '@')) <|> pure ""
  let hostPart ← host
  let portPart ← (attempt (pchar ':' *> port)) <|> pure ""
  return (if userInfo.isEmpty then "" else userInfo ++ "@") ++
    hostPart ++
    (if portPart.isEmpty then "" else ":" ++ portPart)

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
  manyStrings1 (
    pctEncoded
    <|> (·.toString) <$> unreserved
    <|> (·.toString) <$> subDelims
    <|> (·.toString) <$> satisfy (· == '@')
  )

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

/-- query = *( pchar / "/" / "?" ) -/
def query : Parser String :=
  manyStrings (
    pchar
    <|> (·.toString) <$> satisfy (· == '/')
    <|> (·.toString) <$> satisfy (· == '?')
  )

/-- fragment = *( pchar / "/" / "?" ) -/
def fragment : Parser String :=
  manyStrings (
    pchar
    <|> (·.toString) <$> satisfy (· == '/')
    <|> (·.toString) <$> satisfy (· == '?')
  )

/-- hier-part = "//" authority path-abempty
              / path-absolute
              / path-rootless
              / path-empty -/
def hierPart : Parser (Option String × String) :=
  (attempt do
    skipString "//"
    let auth ← authority
    let pathPart ← pathAbempty
    return (some auth, pathPart)
  )
  <|> (attempt do
    let pathPart ← pathAbsolute
    return (none, pathPart)
  )
  <|> (attempt do
    let pathPart ← pathRootless
    return (none, pathPart)
  )
  <|> do
    let pathPart ← pathEmpty
    return (none, pathPart)

/-- scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." ) -/
def scheme : Parser String := do
  let first ← satisfy isAlpha
  let rest ← manyChars (satisfy fun c => isAlpha c || ('0' ≤ c && c ≤ '9') || c == '+' || c == '-' || c == '.')
  return first.toString ++ rest

/-- URI = scheme ":" hier-part [ "?" query ] [ "#" fragment ] -/
def uri : Parser URI := do
  let schemePart ← scheme <|> .fail "Could not parse scheme"
  skipChar ':' <|> .fail "Could not find : in path"
  let (auth, pathPart) ← hierPart <|> .fail "Could not find authority and path"
  let queryPart ← (attempt (skipChar '?' *> query >>= fun q => pure (some q))) <|> pure none
  let fragPart ← (attempt (skipChar '#' *> fragment >>= fun f => pure (some f))) <|> pure none
  return {
    scheme := schemePart
    authority := auth
    path := pathPart
    query := queryPart
    fragment := fragPart
  }

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
    let lastSlash := output.dropRightWhile (· != '/')
    let newOutput := if lastSlash.isEmpty then "" else lastSlash.dropRight 1
    removeDotSegments ("/" ++ input.drop 4) newOutput
  else if input == "/.." then
    let lastSlash := output.dropRightWhile (· != '/')
    let newOutput := if lastSlash.isEmpty then "" else lastSlash.dropRight 1
    removeDotSegments "/" newOutput
  else if input == "." || input == ".." then
    ""
  else
    let afterFirst := input.drop 1
    let charsUntilSlash := afterFirst.takeWhile (· != '/')
    let segmentEnd := charsUntilSlash.length + 1
    let segment := input.take segmentEnd
    removeDotSegments (input.drop segmentEnd) (output ++ segment)

def mergePaths (basePath : String) (refPath : String) (baseHasAuth : Bool) : String :=
  if baseHasAuth && basePath.isEmpty then
    "/" ++ refPath
  else
    let baseDir := basePath.dropRightWhile (· != '/')
    baseDir ++ refPath

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

def uriReference : Parser (URI ⊕ RelativeRef) :=
  (attempt (uri >>= fun u => pure (Sum.inl u)))
  <|> (relativeRef >>= fun r => pure (Sum.inr r))

def parseAndResolve (baseUri : URI) (reference : String) : Except String URI :=
  match uriReference.run reference with
  | .ok (Sum.inl absoluteUri) => .ok absoluteUri
  | .ok (Sum.inr relRef) => .ok (resolve baseUri relRef)
  | .error e => .error e

end LeanUri.Internal
