import LeanUri.URI
import LeanUri.Parsers

namespace LeanUri.Internal

/-! ### 6.2.2.1 Case Normalization -/

/-- Uppercase hexadecimal digits in percent-encodings only -/
partial def uppercasePercentHex (s : String) (acc : String := "") : String :=
  if s.isEmpty then
    acc
  else if s.startsWith "%" && s.length ≥ 3 then
    let h1 := s.get ⟨1⟩
    let h2 := s.get ⟨2⟩
    let normalized := s!"%{h1.toUpper}{h2.toUpper}"
    uppercasePercentHex (s.drop 3) (acc ++ normalized)
  else
    uppercasePercentHex (s.drop 1) (acc ++ (s.get ⟨0⟩).toString)

/-- Normalize the case of a scheme (lowercase) -/
def normalizeSchemeLower (scheme : String) : String :=
  scheme.toLower

/-- Normalize the case of a host component (lowercase for reg-name,
    uppercase hex digits for IPv6) -/
def normalizeHostLower (host : String) : String :=
  if host.startsWith "[" then
    -- IPv6 literal - uppercase only hex digits, keep brackets and colons as-is
    let chars := host.toList.map fun c =>
      if isHexDigit c && isAlpha c then c.toUpper else c
    String.mk chars
  else
    -- reg-name or IPv4 - lowercase
    host.toLower

/-- Normalize an authority component (case only). Assumes the input
    is syntactically valid (e.g., bracketed IPv6, at most one '@', etc.). -/
def normalizeAuthorityCase (auth : String) : String :=
  let parts := auth.splitOn "@"
  match parts with
  | [hostPort] =>
    if hostPort.startsWith "[" then
      -- IPv6 literal with optional ":port" after "]"
      match hostPort.splitOn "]" with
      | ipv6 :: portPart :: _ =>
        let normalizedHost := normalizeHostLower (ipv6 ++ "]")
        if portPart.startsWith ":" then
          normalizedHost ++ portPart
        else
          normalizedHost
      | [ipv6] =>
        normalizeHostLower (ipv6 ++ "]")
      | _ =>
        -- unreachable with parsed input; keep as-is
        normalizeHostLower hostPort
    else
      -- Regular host - optional ":port"
      let hostPortParts := hostPort.splitOn ":"
      match hostPortParts with
      | [host] => normalizeHostLower host
      | host :: port :: _ => normalizeHostLower host ++ ":" ++ port
      | [] => ""
  | userinfo :: hostPort :: _ =>
    let normalizedUserinfo := uppercasePercentHex userinfo
    if hostPort.startsWith "[" then
      match hostPort.splitOn "]" with
      | ipv6 :: portPart :: _ =>
        let normalizedHost := normalizeHostLower (ipv6 ++ "]")
        if portPart.startsWith ":" then
          normalizedUserinfo ++ "@" ++ normalizedHost ++ portPart
        else
          normalizedUserinfo ++ "@" ++ normalizedHost
      | [ipv6] =>
        normalizedUserinfo ++ "@" ++ normalizeHostLower (ipv6 ++ "]")
      | _ =>
        normalizedUserinfo ++ "@" ++ normalizeHostLower hostPort
    else
      let hostPortParts := hostPort.splitOn ":"
      match hostPortParts with
      | [host] => normalizedUserinfo ++ "@" ++ normalizeHostLower host
      | host :: port :: _ => normalizedUserinfo ++ "@" ++ normalizeHostLower host ++ ":" ++ port
      | [] => normalizedUserinfo ++ "@"
  | [] => ""

/-- Case normalization of a URI according to RFC 3986 Section 6.2.2.1.
    - Scheme and host to lowercase
    - Hexadecimal digits in percent-encodings to uppercase -/
def normalizeCase (uri : URI) : URI :=
  {
    scheme := normalizeSchemeLower uri.scheme
    authority := uri.authority.map normalizeAuthorityCase
    path := uppercasePercentHex uri.path
    query := uri.query.map uppercasePercentHex
    fragment := uri.fragment.map uppercasePercentHex
  }

/-! ### 6.2.2.2 Percent-Encoding Normalization -/



/-- Percent-encoding normalization according to RFC 3986 Section 6.2.2.2.
    Decodes percent-encoded octets that correspond to unreserved characters. -/
def normalizePercentEncoding (uri : URI) : URI :=
  {
    scheme := uri.scheme
    authority := uri.authority.map (pctNormalize isUnreserved),
    path := pctNormalize isUnreserved uri.path
    query := uri.query.map (pctNormalize isQueryOrFragmentChar)
    fragment := uri.fragment.map (pctNormalize  isQueryOrFragmentChar)
  }

/-! ### 6.2.2.3 Path Segment Normalization -/

/-- Path segment normalization according to RFC 3986 Section 6.2.2.3.
    Removes dot-segments from the path. -/
def normalizePathSegments (uri : URI) : URI :=
  {
    scheme := uri.scheme
    authority := uri.authority
    path := removeDotSegments uri.path
    query := uri.query
    fragment := uri.fragment
  }

/-! ### 6.2.2 Syntax-Based Normalization -/

/-- This combines:
    - Case normalization (6.2.2.1)
    - Percent-encoding normalization (6.2.2.2)
    - Path segment normalization (6.2.2.3) -/
 def normalizeAll (uri : URI) : URI :=
  uri |> normalizeCase |> normalizePathSegments |> normalizePercentEncoding

end LeanUri.Internal
