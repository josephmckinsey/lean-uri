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

def main : IO Unit := do
  let mut allPassed := true

  IO.println "Running URI Parser Tests"
  IO.println "========================="

  allPassed := (← testPctEncoded) && allPassed
  allPassed := (← testScheme) && allPassed
  allPassed := (← testDecOctet) && allPassed
  allPassed := (← testIPv4Address) && allPassed
  allPassed := (← testRegName) && allPassed

  IO.println "\n========================="
  if allPassed then
    IO.println "✓ All tests passed!"
  else
    IO.println "✗ Some tests failed"
    IO.Process.exit 1
