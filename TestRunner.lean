import LeanUri
import Std.Internal.Parsec.String

open LeanUri
open Std.Internal.Parsec.String

/-! ## Test Helpers -/

def testParser {α : Type} [ToString α] (name : String) (parser : Parser α) (input : String) (expected : String) : IO Bool := do
  match parser.run input with
  | .ok result =>
    let resultStr := toString result
    if resultStr == expected then
      IO.println s!"✓ {name}: '{input}' → {resultStr}"
      return true
    else
      IO.println s!"✗ {name}: '{input}' → got {resultStr}, expected {expected}"
      return false
  | .error err =>
    IO.println s!"✗ {name}: '{input}' → parse error: {err}"
    return false

def expectError {α : Type} (name : String) (parser : Parser α) (input : String) : IO Bool := do
  match parser.run input with
  | .ok _ =>
    IO.println s!"✗ {name}: '{input}' → expected error but got result"
    return false
  | .error _ =>
    IO.println s!"✓ {name}: '{input}' → correctly failed"
    return true

/-- Test parser and ensure all input is consumed -/
def testParserComplete {α : Type} [ToString α] (name : String) (parser : Parser α) (input : String) (expected : String) : IO Bool := do
  let fullParser := parser <* Std.Internal.Parsec.eof
  match fullParser.run input with
  | .ok result =>
    let resultStr := toString result
    if resultStr == expected then
      IO.println s!"✓ {name}: '{input}' → {resultStr}"
      return true
    else
      IO.println s!"✗ {name}: '{input}' → got {resultStr}, expected {expected}"
      return false
  | .error err =>
    IO.println s!"✗ {name}: '{input}' → parse error: {err}"
    return false

/-- Expect error when parsing complete input -/
def expectErrorComplete {α : Type} (name : String) (parser : Parser α) (input : String) : IO Bool := do
  let fullParser := parser <* Std.Internal.Parsec.eof
  match fullParser.run input with
  | .ok _ =>
    IO.println s!"✗ {name}: '{input}' → expected error but got result"
    return false
  | .error _ =>
    IO.println s!"✓ {name}: '{input}' → correctly failed"
    return true

/-! ## Tests -/

def testPctEncoded : IO Bool := do
  IO.println "\n=== Testing pct-encoded ==="
  let mut allPassed := true

  -- Valid percent-encoded octets
  allPassed := (← testParser "percent space" pctEncoded "%20" "%20") && allPassed
  allPassed := (← testParser "percent A" pctEncoded "%41" "%41") && allPassed
  allPassed := (← testParser "lowercase hex" pctEncoded "%7e" "%7e") && allPassed
  allPassed := (← testParser "uppercase hex" pctEncoded "%C3" "%C3") && allPassed

  -- Invalid cases
  allPassed := (← expectError "missing hex digit" pctEncoded "%2") && allPassed
  allPassed := (← expectError "invalid hex" pctEncoded "%GG") && allPassed

  return allPassed

def testScheme : IO Bool := do
  IO.println "\n=== Testing scheme ==="
  let mut allPassed := true

  -- Valid schemes (RFC 3986 examples)
  allPassed := (← testParser "http" scheme "http" "http") && allPassed
  allPassed := (← testParser "https" scheme "https" "https") && allPassed
  allPassed := (← testParser "ftp" scheme "ftp" "ftp") && allPassed
  allPassed := (← testParser "scheme with plus" scheme "svn+ssh" "svn+ssh") && allPassed
  allPassed := (← testParser "scheme with dot" scheme "x.y" "x.y") && allPassed
  allPassed := (← testParser "scheme with dash" scheme "z-39.50" "z-39.50") && allPassed

  -- Invalid schemes (must start with alpha)
  allPassed := (← expectError "starts with digit" scheme "9abc") && allPassed
  allPassed := (← expectError "starts with dash" scheme "-abc") && allPassed

  return allPassed

def testDecOctet : IO Bool := do
  IO.println "\n=== Testing dec-octet ==="
  let mut allPassed := true

  -- Valid octets
  allPassed := (← testParser "zero" decOctet "0" "0") && allPassed
  allPassed := (← testParser "single digit" decOctet "5" "5") && allPassed
  allPassed := (← testParser "two digits" decOctet "42" "42") && allPassed
  allPassed := (← testParser "100-199" decOctet "150" "150") && allPassed
  allPassed := (← testParser "200-249" decOctet "200" "200") && allPassed
  allPassed := (← testParser "250-255" decOctet "255" "255") && allPassed

  -- Invalid octets (>255)
  allPassed := (← expectError "256" decOctet "256") && allPassed
  allPassed := (← expectError "300" decOctet "300") && allPassed
  allPassed := (← expectError "999" decOctet "999") && allPassed

  return allPassed

def testIPv4Address : IO Bool := do
  IO.println "\n=== Testing IPv4address ==="
  let mut allPassed := true

  -- Valid IPv4 addresses
  allPassed := (← testParser "localhost" ipv4address "127.0.0.1" "127.0.0.1") && allPassed
  allPassed := (← testParser "google dns" ipv4address "8.8.8.8" "8.8.8.8") && allPassed
  allPassed := (← testParser "private network" ipv4address "192.168.1.1" "192.168.1.1") && allPassed
  allPassed := (← testParser "max values" ipv4address "255.255.255.255" "255.255.255.255") && allPassed

  -- Invalid addresses
  allPassed := (← expectError "octet > 255" ipv4address "192.168.1.256") && allPassed
  allPassed := (← expectError "missing octet" ipv4address "192.168.1") && allPassed

  return allPassed

def testRegName : IO Bool := do
  IO.println "\n=== Testing reg-name ==="
  let mut allPassed := true

  -- Valid registered names
  allPassed := (← testParser "simple domain" regName "example.com" "example.com") && allPassed
  allPassed := (← testParser "subdomain" regName "www.example.com" "www.example.com") && allPassed
  allPassed := (← testParser "with dash" regName "my-site.org" "my-site.org") && allPassed
  allPassed := (← testParser "with tilde" regName "user~name" "user~name") && allPassed
  allPassed := (← testParser "percent encoded" regName "ex%20ample" "ex%20ample") && allPassed
  allPassed := (← testParser "empty" regName "" "") && allPassed

  return allPassed

def testPort : IO Bool := do
  IO.println "\n=== Testing port ==="
  let mut allPassed := true

  -- Valid ports
  allPassed := (← testParser "http port" port "80" "80") && allPassed
  allPassed := (← testParser "https port" port "443" "443") && allPassed
  allPassed := (← testParser "custom port" port "8080" "8080") && allPassed
  allPassed := (← testParser "empty port" port "" "") && allPassed

  -- Invalid ports (should fail with non-digits)
  allPassed := (← expectErrorComplete "port with letter" port "80a0") && allPassed
  allPassed := (← expectErrorComplete "port with dash" port "80-80") && allPassed

  return allPassed

def testHost : IO Bool := do
  IO.println "\n=== Testing host ==="
  let mut allPassed := true

  -- IPv4 addresses (should be parsed as IPv4)
  allPassed := (← testParser "IPv4 localhost" host "127.0.0.1" "127.0.0.1") && allPassed
  allPassed := (← testParser "IPv4 address" host "192.168.1.1" "192.168.1.1") && allPassed

  -- Registered names
  allPassed := (← testParser "domain" host "example.com" "example.com") && allPassed
  allPassed := (← testParser "subdomain" host "www.example.com" "www.example.com") && allPassed
  allPassed := (← testParser "localhost name" host "localhost" "localhost") && allPassed

  -- Invalid hosts
  -- Note: "256.0.0.1" is valid as a reg-name even though it's not a valid IPv4
  allPassed := (← testParserComplete "looks like IPv4 but is reg-name" host "256.0.0.1" "256.0.0.1") && allPassed
  allPassed := (← expectErrorComplete "host with space" host "exam ple.com") && allPassed
  allPassed := (← expectErrorComplete "host with @" host "ex@mple.com") && allPassed

  return allPassed

def testAuthority : IO Bool := do
  IO.println "\n=== Testing authority ==="
  let mut allPassed := true

  -- Just host
  allPassed := (← testParser "host only" authority "example.com" "example.com") && allPassed

  -- Host with port
  allPassed := (← testParser "host:port" authority "example.com:80" "example.com:80") && allPassed
  allPassed := (← testParser "IPv4:port" authority "127.0.0.1:8080" "127.0.0.1:8080") && allPassed

  -- Userinfo with host
  allPassed := (← testParser "user@host" authority "user@example.com" "user@example.com") && allPassed
  allPassed := (← testParser "user:pass@host" authority "user:pass@example.com" "user:pass@example.com") && allPassed

  -- Full authority
  allPassed := (← testParser "user@host:port" authority "admin@example.com:8080" "admin@example.com:8080") && allPassed
  allPassed := (← testParser "user:pass@host:port" authority "user:pass@localhost:3000" "user:pass@localhost:3000") && allPassed

  -- Invalid authority
  allPassed := (← expectErrorComplete "multiple @" authority "user@pass@host") && allPassed
  -- Note: "256.0.0.1" is valid as a reg-name, so this should succeed
  allPassed := (← testParserComplete "looks like IPv4 in authority" authority "user@256.0.0.1:80" "user@256.0.0.1:80") && allPassed
  allPassed := (← expectErrorComplete "port with letters" authority "example.com:80a") && allPassed

  return allPassed

def testPath : IO Bool := do
  IO.println "\n=== Testing path components ==="
  let mut allPassed := true

  -- path-abempty (begins with "/" or is empty)
  allPassed := (← testParserComplete "empty path" pathAbempty "" "") && allPassed
  allPassed := (← testParserComplete "single segment" pathAbempty "/foo" "/foo") && allPassed
  allPassed := (← testParserComplete "multiple segments" pathAbempty "/foo/bar" "/foo/bar") && allPassed
  allPassed := (← testParserComplete "with percent-encoding" pathAbempty "/foo%20bar/baz" "/foo%20bar/baz") && allPassed
  allPassed := (← testParserComplete "with special chars" pathAbempty "/foo:bar/@test" "/foo:bar/@test") && allPassed

  -- path-absolute (begins with "/" but not "//")
  allPassed := (← testParserComplete "absolute root" pathAbsolute "/" "/") && allPassed
  allPassed := (← testParserComplete "absolute path" pathAbsolute "/foo/bar" "/foo/bar") && allPassed
  allPassed := (← testParserComplete "absolute with colon" pathAbsolute "/foo:bar" "/foo:bar") && allPassed

  -- path-rootless (begins with a segment)
  allPassed := (← testParserComplete "rootless single" pathRootless "foo" "foo") && allPassed
  allPassed := (← testParserComplete "rootless multiple" pathRootless "foo/bar" "foo/bar") && allPassed
  allPassed := (← testParserComplete "rootless with colon" pathRootless "foo:bar/baz" "foo:bar/baz") && allPassed

  -- path-noscheme (begins with non-colon segment)
  allPassed := (← testParserComplete "noscheme simple" pathNoscheme "foo" "foo") && allPassed
  allPassed := (← testParserComplete "noscheme multiple" pathNoscheme "foo/bar" "foo/bar") && allPassed
  allPassed := (← testParserComplete "noscheme colon in second" pathNoscheme "foo/bar:baz" "foo/bar:baz") && allPassed

  -- Invalid paths
  allPassed := (← expectErrorComplete "noscheme with colon in first" pathNoscheme "foo:bar") && allPassed
  allPassed := (← expectErrorComplete "path with space" pathAbsolute "/foo bar") && allPassed
  allPassed := (← expectErrorComplete "path with hash" pathAbsolute "/foo#bar") && allPassed
  allPassed := (← expectErrorComplete "path with question" pathAbsolute "/foo?bar") && allPassed
  allPassed := (← expectErrorComplete "path with bracket" pathAbsolute "/foo[bar]") && allPassed

  return allPassed

def testQuery : IO Bool := do
  IO.println "\n=== Testing query ==="
  let mut allPassed := true

  allPassed := (← testParserComplete "empty query" query "" "") && allPassed
  allPassed := (← testParserComplete "simple query" query "name=value" "name=value") && allPassed
  allPassed := (← testParserComplete "multiple params" query "a=1&b=2" "a=1&b=2") && allPassed
  allPassed := (← testParserComplete "with slash" query "path/to/resource" "path/to/resource") && allPassed
  allPassed := (← testParserComplete "with question mark" query "a=1?b=2" "a=1?b=2") && allPassed
  allPassed := (← testParserComplete "percent-encoded" query "name=hello%20world" "name=hello%20world") && allPassed
  allPassed := (← testParserComplete "with colon" query "key:value" "key:value") && allPassed
  allPassed := (← testParserComplete "with @" query "user@domain" "user@domain") && allPassed

  -- Invalid query
  allPassed := (← expectErrorComplete "query with space" query "name=hello world") && allPassed
  allPassed := (← expectErrorComplete "query with hash" query "name=value#section") && allPassed
  allPassed := (← expectErrorComplete "query with bracket" query "arr[0]=value") && allPassed

  return allPassed

def testFragment : IO Bool := do
  IO.println "\n=== Testing fragment ==="
  let mut allPassed := true

  allPassed := (← testParserComplete "empty fragment" fragment "" "") && allPassed
  allPassed := (← testParserComplete "simple fragment" fragment "section1" "section1") && allPassed
  allPassed := (← testParserComplete "with slash" fragment "section/subsection" "section/subsection") && allPassed
  allPassed := (← testParserComplete "with question mark" fragment "section?param=value" "section?param=value") && allPassed
  allPassed := (← testParserComplete "percent-encoded" fragment "sec%20tion" "sec%20tion") && allPassed
  allPassed := (← testParserComplete "with colon" fragment "line:42" "line:42") && allPassed
  allPassed := (← testParserComplete "with @" fragment "user@note" "user@note") && allPassed

  -- Invalid fragment
  allPassed := (← expectErrorComplete "fragment with space" fragment "sec tion") && allPassed
  allPassed := (← expectErrorComplete "fragment with hash" fragment "section#subsection") && allPassed
  allPassed := (← expectErrorComplete "fragment with bracket" fragment "item[0]") && allPassed

  return allPassed

def testURI : IO Bool := do
  IO.println "\n=== Testing complete URI ==="
  let mut allPassed := true

  -- Simple URIs
  allPassed := (← testParserComplete "http URL" uri "http://example.com"
    "http://example.com") && allPassed
  allPassed := (← testParserComplete "https with path" uri "https://example.com/path"
    "https://example.com/path") && allPassed
  allPassed := (← testParserComplete "with port" uri "http://example.com:8080"
    "http://example.com:8080") && allPassed

  -- With query
  allPassed := (← testParserComplete "with query" uri "http://example.com?key=value"
    "http://example.com?key=value") && allPassed
  allPassed := (← testParserComplete "with path and query" uri "http://example.com/path?key=value"
    "http://example.com/path?key=value") && allPassed

  -- With fragment
  allPassed := (← testParserComplete "with fragment" uri "http://example.com#section"
    "http://example.com#section") && allPassed
  allPassed := (← testParserComplete "with query and fragment" uri "http://example.com?key=value#section"
    "http://example.com?key=value#section") && allPassed

  -- With authority components
  allPassed := (← testParserComplete "with userinfo" uri "http://user@example.com/path"
    "http://user@example.com/path") && allPassed
  allPassed := (← testParserComplete "with user:pass" uri "ftp://user:pass@ftp.example.com/file.txt"
    "ftp://user:pass@ftp.example.com/file.txt") && allPassed
  allPassed := (← testParserComplete "full authority" uri "http://admin:secret@example.com:8080/admin"
    "http://admin:secret@example.com:8080/admin") && allPassed

  -- Complex URIs
  allPassed := (← testParserComplete "complex URI" uri
    "https://user@example.com:443/path/to/resource?query=param&foo=bar#section1"
    "https://user@example.com:443/path/to/resource?query=param&foo=bar#section1") && allPassed

  -- IPv4 addresses
  allPassed := (← testParserComplete "IPv4 host" uri "http://192.168.1.1/path"
    "http://192.168.1.1/path") && allPassed
  allPassed := (← testParserComplete "localhost IP" uri "http://127.0.0.1:8080"
    "http://127.0.0.1:8080") && allPassed

  -- URIs without authority
  allPassed := (← testParserComplete "mailto" uri "mailto:user@example.com"
    "mailto:user@example.com") && allPassed
  allPassed := (← testParserComplete "file scheme" uri "file:/path/to/file"
    "file:/path/to/file") && allPassed
  allPassed := (← testParserComplete "urn" uri "urn:isbn:0451450523"
    "urn:isbn:0451450523") && allPassed

  -- Edge cases
  allPassed := (← testParserComplete "empty path" uri "http://example.com"
    "http://example.com") && allPassed
  allPassed := (← testParserComplete "root path" uri "http://example.com/"
    "http://example.com/") && allPassed
  allPassed := (← testParserComplete "percent-encoded in path" uri "http://example.com/hello%20world"
    "http://example.com/hello%20world") && allPassed

  -- Invalid URIs
  allPassed := (← expectErrorComplete "missing colon" uri "http//example.com") && allPassed
  allPassed := (← expectErrorComplete "invalid scheme start" uri "9http://example.com") && allPassed
  allPassed := (← expectErrorComplete "space in host" uri "http://exam ple.com") && allPassed
  allPassed := (← expectErrorComplete "space in path" uri "http://example.com/hello world") && allPassed
  allPassed := (← expectErrorComplete "invalid port" uri "http://example.com:80a/path") && allPassed
  allPassed := (← expectErrorComplete "multiple @" uri "http://user@pass@host/path") && allPassed

  return allPassed

-- Helper to parse base URI for resolution tests
def parseBaseURI (baseStr : String) : IO (Option URI) := do
  match uri.run baseStr with
  | .ok u => pure (some u)
  | .error e =>
    IO.println s!"✗ Failed to parse base URI: {e}"
    pure none

-- Helper to test URI reference resolution
def testResolve (base : URI) (name : String) (refStr : String) (expected : String) : IO Bool := do
  match parseAndResolve base refStr with
  | .ok result =>
    let resultStr := toString result
    if resultStr == expected then
      IO.println s!"✓ {name}: '{refStr}' → {resultStr}"
      return true
    else
      IO.println s!"✗ {name}: '{refStr}' → got {resultStr}, expected {expected}"
      return false
  | .error err =>
    IO.println s!"✗ {name}: '{refStr}' → error: {err}"
    return false

def testRelativeResolution : IO Bool := do
  IO.println "\n=== Testing Relative Reference Resolution (RFC 3986 Section 5.4) ==="

  -- Base URI for all tests (from RFC 3986 Section 5.4)
  let some base ← parseBaseURI "http://a/b/c/d;p?q" | return false
  let mut allPassed := true

  IO.println "\n--- Normal Examples (RFC 3986 Section 5.4.1) ---"

  -- Different scheme
  allPassed := (← testResolve base "different scheme" "g:h" "g:h") && allPassed

  -- Relative path references
  allPassed := (← testResolve base "simple relative" "g" "http://a/b/c/g") && allPassed
  allPassed := (← testResolve base "current directory" "./g" "http://a/b/c/g") && allPassed
  allPassed := (← testResolve base "relative directory" "g/" "http://a/b/c/g/") && allPassed

  -- Absolute path reference
  allPassed := (← testResolve base "absolute path" "/g" "http://a/g") && allPassed

  -- Network-path reference
  allPassed := (← testResolve base "network-path" "//g" "http://g") && allPassed

  -- Query and fragment
  allPassed := (← testResolve base "query replacement" "?y" "http://a/b/c/d;p?y") && allPassed
  allPassed := (← testResolve base "relative with query" "g?y" "http://a/b/c/g?y") && allPassed
  allPassed := (← testResolve base "fragment only" "#s" "http://a/b/c/d;p?q#s") && allPassed
  allPassed := (← testResolve base "relative with fragment" "g#s" "http://a/b/c/g#s") && allPassed
  allPassed := (← testResolve base "query and fragment" "g?y#s" "http://a/b/c/g?y#s") && allPassed

  -- Semicolon in path
  allPassed := (← testResolve base "semicolon segment" ";x" "http://a/b/c/;x") && allPassed
  allPassed := (← testResolve base "relative with semicolon" "g;x" "http://a/b/c/g;x") && allPassed
  allPassed := (← testResolve base "semicolon query fragment" "g;x?y#s" "http://a/b/c/g;x?y#s") && allPassed

  -- Empty reference
  allPassed := (← testResolve base "empty reference" "" "http://a/b/c/d;p?q") && allPassed

  -- Dot segments
  allPassed := (← testResolve base "current dir only" "." "http://a/b/c/") && allPassed
  allPassed := (← testResolve base "current dir with slash" "./" "http://a/b/c/") && allPassed
  allPassed := (← testResolve base "parent dir" ".." "http://a/b/") && allPassed
  allPassed := (← testResolve base "parent dir with slash" "../" "http://a/b/") && allPassed
  allPassed := (← testResolve base "parent then relative" "../g" "http://a/b/g") && allPassed
  allPassed := (← testResolve base "two parents" "../.." "http://a/") && allPassed
  allPassed := (← testResolve base "two parents with slash" "../../" "http://a/") && allPassed
  allPassed := (← testResolve base "two parents then relative" "../../g" "http://a/g") && allPassed

  IO.println "\n--- Abnormal Examples (RFC 3986 Section 5.4.2) ---"

  -- Excessive parent references
  allPassed := (← testResolve base "three parents" "../../../g" "http://a/g") && allPassed
  allPassed := (← testResolve base "four parents" "../../../../g" "http://a/g") && allPassed

  -- Dot segments in absolute paths
  allPassed := (← testResolve base "abs with current" "/./g" "http://a/g") && allPassed
  allPassed := (← testResolve base "abs with parent" "/../g" "http://a/g") && allPassed

  -- Dot-like segments (not complete dots)
  allPassed := (← testResolve base "dot suffix" "g." "http://a/b/c/g.") && allPassed
  allPassed := (← testResolve base "dot prefix" ".g" "http://a/b/c/.g") && allPassed
  allPassed := (← testResolve base "dotdot suffix" "g.." "http://a/b/c/g..") && allPassed
  allPassed := (← testResolve base "dotdot prefix" "..g" "http://a/b/c/..g") && allPassed

  -- Unnecessary dot segments
  allPassed := (← testResolve base "unnecessary dots" "./../g" "http://a/b/g") && allPassed
  allPassed := (← testResolve base "relative with trailing dot" "./g/." "http://a/b/c/g/") && allPassed
  allPassed := (← testResolve base "mid-path current dir" "g/./h" "http://a/b/c/g/h") && allPassed
  allPassed := (← testResolve base "mid-path parent dir" "g/../h" "http://a/b/c/h") && allPassed
  allPassed := (← testResolve base "semicolon with dots 1" "g;x=1/./y" "http://a/b/c/g;x=1/y") && allPassed
  allPassed := (← testResolve base "semicolon with dots 2" "g;x=1/../y" "http://a/b/c/y") && allPassed

  -- Query and fragment with dot segments (should NOT be processed)
  allPassed := (← testResolve base "query with dots 1" "g?y/./x" "http://a/b/c/g?y/./x") && allPassed
  allPassed := (← testResolve base "query with dots 2" "g?y/../x" "http://a/b/c/g?y/../x") && allPassed
  allPassed := (← testResolve base "fragment with dots 1" "g#s/./x" "http://a/b/c/g#s/./x") && allPassed
  allPassed := (← testResolve base "fragment with dots 2" "g#s/../x" "http://a/b/c/g#s/../x") && allPassed

  -- Backward compatibility (non-strict parsing)
  allPassed := (← testResolve base "same scheme colon" "http:g" "http:g") && allPassed

  return allPassed

def main : IO Unit := do
  let mut allPassed := true

  IO.println "Running URI Parser Tests"
  IO.println "========================="

  allPassed := (← testPctEncoded) && allPassed
  allPassed := (← testScheme) && allPassed
  allPassed := (← testDecOctet) && allPassed
  allPassed := (← testIPv4Address) && allPassed
  allPassed := (← testRegName) && allPassed
  allPassed := (← testPort) && allPassed
  allPassed := (← testHost) && allPassed
  allPassed := (← testAuthority) && allPassed
  allPassed := (← testPath) && allPassed
  allPassed := (← testQuery) && allPassed
  allPassed := (← testFragment) && allPassed
  allPassed := (← testURI) && allPassed
  allPassed := (← testRelativeResolution) && allPassed

  IO.println "\n========================="
  if allPassed then
    IO.println "✓ All tests passed!"
  else
    IO.println "✗ Some tests failed"
    IO.Process.exit 1
