import LeanUri.URI
import LeanUri.Parsers

namespace LeanUri

/-! ## Normalization (RFC 3986 Section 6.2) -/

/-- Convert a hexadecimal character to its numeric value -/
def hexCharToNat (c : Char) : Option Nat :=
  if '0' ≤ c && c ≤ '9' then
    some (c.toNat - '0'.toNat)
  else if 'A' ≤ c && c ≤ 'F' then
    some (c.toNat - 'A'.toNat + 10)
  else if 'a' ≤ c && c ≤ 'f' then
    some (c.toNat - 'a'.toNat + 10)
  else
    none

/-- Convert a natural number (0-15) to uppercase hexadecimal character -/
def natToHexChar (n : Nat) : Char :=
  if n < 10 then
    Char.ofNat ('0'.toNat + n)
  else
    Char.ofNat ('A'.toNat + (n - 10))

/-- Decode a percent-encoded triplet to its character value -/
def decodePercentEncoded (h1 h2 : Char) : Option Char :=
  match hexCharToNat h1, hexCharToNat h2 with
  | some v1, some v2 => some (Char.ofNat (v1 * 16 + v2))
  | _, _ => none

/-! ### 6.2.2.1 Case Normalization -/

/-- Uppercase hexadecimal digits in percent-encodings only -/
partial def uppercasePercentHex (s : String) (acc : String := "") : String :=
  if s.isEmpty then
    acc
  else if s.startsWith "%" && s.length ≥ 3 then
    let h1 := s.get ⟨1⟩
    let h2 := s.get ⟨2⟩
    let v1 := (hexCharToNat h1).getD 0
    let v2 := (hexCharToNat h2).getD 0
    let normalized := "%" ++ (natToHexChar v1).toString ++ (natToHexChar v2).toString
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

/-- Normalize an authority component (case only) -/
def normalizeAuthorityCase (auth : String) : String :=
  -- Split authority into userinfo@host:port
  let parts := auth.splitOn "@"
  match parts with
  | [hostPort] =>
    -- No userinfo
    -- Check if this is an IPv6 literal (starts with [)
    if hostPort.startsWith "[" then
      -- IPv6 literal - find the closing bracket
      match hostPort.splitOn "]" with
      | ipv6 :: portPart :: _ =>
        -- ipv6 includes the opening bracket
        let normalizedHost := normalizeHostLower (ipv6 ++ "]")
        if portPart.startsWith ":" then
          normalizedHost ++ portPart
        else if portPart.isEmpty then
          normalizedHost
        else
          normalizedHost ++ portPart
      | [ipv6] =>
        -- No closing bracket found or no port
        normalizeHostLower (ipv6 ++ "]")
      | _ => normalizeHostLower hostPort
    else
      -- Regular host - split by colon for port
      let hostPortParts := hostPort.splitOn ":"
      match hostPortParts with
      | [host] => normalizeHostLower host
      | host :: port :: _ =>
        normalizeHostLower host ++ ":" ++ port
      | [] => ""
  | userinfo :: hostPort :: _ =>
    -- Has userinfo
    let normalizedUserinfo := uppercasePercentHex userinfo
    -- Check if this is an IPv6 literal
    if hostPort.startsWith "[" then
      match hostPort.splitOn "]" with
      | ipv6 :: portPart :: _ =>
        let normalizedHost := normalizeHostLower (ipv6 ++ "]")
        if portPart.startsWith ":" then
          normalizedUserinfo ++ "@" ++ normalizedHost ++ portPart
        else if portPart.isEmpty then
          normalizedUserinfo ++ "@" ++ normalizedHost
        else
          normalizedUserinfo ++ "@" ++ normalizedHost ++ portPart
      | [ipv6] =>
        normalizedUserinfo ++ "@" ++ normalizeHostLower (ipv6 ++ "]")
      | _ => normalizedUserinfo ++ "@" ++ normalizeHostLower hostPort
    else
      let hostPortParts := hostPort.splitOn ":"
      match hostPortParts with
      | [host] => normalizedUserinfo ++ "@" ++ normalizeHostLower host
      | host :: port :: _ =>
        normalizedUserinfo ++ "@" ++ normalizeHostLower host ++ ":" ++ port
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

/-- Decode percent-encoded unreserved characters -/
partial def decodeUnreserved (s : String) (acc : String := "") : String :=
  if s.isEmpty then
    acc
  else if s.startsWith "%" && s.length ≥ 3 then
    let h1 := s.get ⟨1⟩
    let h2 := s.get ⟨2⟩
    match decodePercentEncoded h1 h2 with
    | some c =>
      -- If the decoded character is unreserved, use it directly
      if isUnreserved c then
        decodeUnreserved (s.drop 3) (acc ++ c.toString)
      else
        -- Otherwise, keep the percent-encoding as-is
        decodeUnreserved (s.drop 3) (acc ++ "%" ++ h1.toString ++ h2.toString)
    | none =>
      -- Invalid percent-encoding, keep as-is
      decodeUnreserved (s.drop 1) (acc ++ (s.get ⟨0⟩).toString)
  else
    decodeUnreserved (s.drop 1) (acc ++ (s.get ⟨0⟩).toString)

/-- Percent-encoding normalization according to RFC 3986 Section 6.2.2.2.
    Decodes percent-encoded octets that correspond to unreserved characters. -/
def normalizePercentEncoding (uri : URI) : URI :=
  {
    scheme := uri.scheme
    authority := uri.authority.map decodeUnreserved
    path := decodeUnreserved uri.path
    query := uri.query.map decodeUnreserved
    fragment := uri.fragment.map decodeUnreserved
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

/-- Syntax-based normalization of a URI according to RFC 3986 Section 6.2.2.
    This combines:
    - Case normalization (6.2.2.1)
    - Percent-encoding normalization (6.2.2.2)
    - Path segment normalization (6.2.2.3) -/
def URI.normalize (uri : URI) : URI :=
  uri |> normalizeCase |> normalizePathSegments |> normalizePercentEncoding

end LeanUri
