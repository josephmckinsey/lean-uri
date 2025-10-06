import LeanUri
import LeanUri.Normalization
import UriTesting.Helpers

open LeanUri
open Internal
open Testing

def testNormalization (normalizer : URI → URI) (input : String) (output : String) : Bool :=
  match uri.run input with
  | .ok x => toString (normalizer x) == output
  | _ => false

def testNormalizeCase : TestM Unit := testFunction "Case Normalization (6.2.2.1)" do
  test "scheme lowercase" (testNormalization normalizeCase
    "HTTP://www.example.com/"
    "http://www.example.com/")

  test "host lowercase" (testNormalization normalizeCase
    "http://WWW.EXAMPLE.COM/"
    "http://www.example.com/")

  test "hex uppercase in percent-encoding" (testNormalization normalizeCase
    "http://example.com/%3a"
    "http://example.com/%3A")

  test "combined case" (testNormalization normalizeCase
    "HTTP://WWW.EXAMPLE.COM/%3a%7b"
    "http://www.example.com/%3A%7B")

  test "IPv6 uppercase" (testNormalization normalizeCase
    "http://[2001:db8::1]/"
    "http://[2001:DB8::1]/")

def testNormalizePercentEncoding : TestM Unit := testFunction "Percent-Encoding Normalization (6.2.2.2)" do
  test "decode unreserved hyphen" (testNormalization normalizePercentEncoding
    "http://example.com/%2D"
    "http://example.com/-")

  test "decode unreserved period" (testNormalization normalizePercentEncoding
    "http://example.com/%2E"
    "http://example.com/.")

  test "decode unreserved underscore" (testNormalization normalizePercentEncoding
    "http://example.com/%5F"
    "http://example.com/_")

  test "decode unreserved tilde" (testNormalization normalizePercentEncoding
    "http://example.com/%7E"
    "http://example.com/~")

  test "decode unreserved alpha" (testNormalization normalizePercentEncoding
    "http://example.com/%41"
    "http://example.com/A")

  test "decode unreserved digit" (testNormalization normalizePercentEncoding
    "http://example.com/%30"
    "http://example.com/0")

  test "keep reserved colon encoded" (testNormalization normalizePercentEncoding
    "http://example.com/%3A"
    "http://example.com/%3A")

  test "keep reserved slash encoded" (testNormalization normalizePercentEncoding
    "http://example.com/%2F"
    "http://example.com/%2F")

def testNormalizePathSegments : TestM Unit := testFunction "Path Segment Normalization (6.2.2.3)" do
  test "remove single dot" (testNormalization normalizePathSegments
    "http://example.com/./path"
    "http://example.com/path")

  test "remove double dot" (testNormalization normalizePathSegments
    "http://example.com/a/../path"
    "http://example.com/path")

  test "remove multiple dots" (testNormalization normalizePathSegments
    "http://example.com/a/b/c/../../d"
    "http://example.com/a/d")

  test "remove trailing dot" (testNormalization normalizePathSegments
    "http://example.com/path/."
    "http://example.com/path/")

  test "remove trailing double dot" (testNormalization normalizePathSegments
    "http://example.com/a/b/.."
    "http://example.com/a/")

def testNormalizeSyntax : TestM Unit := testFunction "Full Normalization" do
  -- RFC 3986 Section 6.2.2 example
  test "RFC 3986 example" (testNormalization URI.normalize
    "eXAMPLE://a/./b/../b/%63/%7bfoo%7d"
    "example://a/b/c/%7Bfoo%7D")

  -- Combined normalization
  test "combined case and percent" (testNormalization URI.normalize
    "HTTP://Example.COM/%7Euser"
    "http://example.com/~user")

  test "combined all normalizations" (testNormalization URI.normalize
    "HTTP://Example.COM:80/./a/../b/%7Euser/%2D"
    "http://example.com:80/b/~user/-")

  test "case then path then percent" (testNormalization URI.normalize
    "HTTP://EXAMPLE.COM/a/./b/../c/%41%42%43"
    "http://example.com/a/c/ABC")


-- Percent-encoding/decoding tests for both unreserved and unicode modes
def testPercentEncoding : TestM Unit := testFunction "Percent Encoding/Decoding" do
  return

-- Add to all tests
def allNormalizationTests : TestM Unit := group "Normalization" do
  testNormalizeCase
  testNormalizePercentEncoding
  testNormalizePathSegments
  testNormalizeSyntax
  testPercentEncoding
