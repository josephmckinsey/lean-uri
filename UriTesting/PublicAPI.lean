import LeanUri.Basic
import UriTesting.Helpers
open LeanUri
open Testing

/-- info: "https://example.com/a/c?x#y" -/
#guard_msgs in
#eval match URI.parse "https://EXAMPLE.com/a/./b/../c?x#y" with
| .ok u   => (URI.normalize u).toString
| .error e => s!"error: {e}"

def base : URI := { scheme := "https", authority := some "example.com", path := "/dir/index.html" }
/-- info: Except.ok "https://example.com/x" -/
#guard_msgs in
#eval (URI.resolve base "../x").map (·.toString)

def testURIParse : TestM Unit := testFunction "URI.parse" do
  testEq "parse simple URI" (URI.parse "http://a/b?c#d").isOk true
  testEq "parse invalid URI" (URI.parse "not a uri").isOk false

def testRelativeRefParse : TestM Unit := testFunction "RelativeRef.parse" do
  testEq "parse simple relative" (RelativeRef.parse "/foo/bar?x#y").isOk true
  testEq "parse weird relative" (RelativeRef.parse ":bad").isOk true

def testParseReference : TestM Unit := testFunction "parseReference" do
  testEq "parseReference absolute" (parseReference "mailto:user@example.com").isOk true
  testEq "parseReference relative" (parseReference "./a/b").isOk true

def testURIToString : TestM Unit := testFunction "URI.toString" do
  let u := URI.encode "http" (some "host") "/p" (some "q") (some "f")
  testEq "URI.toString" (u.toString) "http://host/p?q#f"

def testRelativeRefToString : TestM Unit := testFunction "RelativeRef.toString" do
  let r : RelativeRef := { authority := some "host", path := "/p", query := some "q", fragment := some "f" }
  testEq "RelativeRef.toString" (r.toString) "//host/p?q#f"

def testURINormalize : TestM Unit := testFunction "URI.normalize" do
  let u := URI.encode "http" (some "host") "/p" (some "q") (some "f")
  let u2 := URI.parse "HTTP://EXAMPLE.com/a/./b/../c" |>.toOption.getD u
  testEq "URI.normalize" (URI.normalize u2).toString "http://example.com/a/c"

def testURIResolve : TestM Unit := testFunction "URI.resolve" do
  let base := URI.encode "http" (some "host") "/dir/index.html"
  testEq "URI.resolve relative" ((URI.resolve base "../x").toOption.map (λ u => u.toString)) (some "http://host/x")

def testURIResolveRef : TestM Unit := testFunction "URI.resolveRef" do
  let base := URI.encode "http" (some "host") "/dir/index.html"
  let rr : RelativeRef := { authority := none, path := "../x", query := none, fragment := none }
  testEq "URI.resolveRef" ((URI.resolveRef base rr).toString) "http://host/x"

def testURIIsAbsolute : TestM Unit := testFunction "URI.isAbsolute" do
  let u := URI.encode "http" (some "host") "/p" (some "q") (some "f")
  testEq "isAbsolute true" (URI.isAbsolute u) true
  testEq "isAbsolute false" (URI.isAbsolute (URI.encode "mailto" none "foo@bar")) false

def testURIMkEncoded : TestM Unit := testFunction "URI.mkEncoded" do
  testEq "URI.mkEncoded encodes" (URI.encode "http" (some "user name@host") "/foo bar" (some "q=1 2") (some "frag ment")).toString "http://user%20name@host/foo%20bar?q=1%202#frag%20ment"

def testRelativeRefMkEncoded : TestM Unit := testFunction "RelativeRef.mkEncoded" do
  testEq "authority only" (RelativeRef.encode (some "user:pass@example.com")).toString "//user:pass@example.com"
  testEq "path only" (RelativeRef.encode none "/foo bar").toString "/foo%20bar"
  testEq "query only" (RelativeRef.encode none "" (some "a=1&b=2")).toString "?a=1&b=2"
  testEq "fragment only" (RelativeRef.encode none "" none (some "frag ment")).toString "#frag%20ment"
  testEq "all components" (RelativeRef.encode (some "user@host:80") "/p ath" (some "q=1 2") (some "frag#")).toString "//user@host:80/p%20ath?q=1%202#frag%23"
  testEq "unreserved chars" (RelativeRef.encode none "/azAZ09-._~").toString "/azAZ09-._~"
  testEq "reserved chars in path" (RelativeRef.encode none "/:@!$&'()*+,;=").toString "/:@!$&'()*+,;="
  testEq "space in authority" (RelativeRef.encode (some "user name@host")).toString "//user%20name@host"

def testURIEquivalent : TestM Unit := testFunction "URI.equivalent" do
  let u3 := URI.encode "http" (some "EXAMPLE.com") "/a/./b/../c"
  let u4 := URI.encode "http" (some "example.COM") "/a/c"
  testEq "URI.equivalent" (URI.equivalent u3 u4) true

def testURIGetAccessors : TestM Unit := testFunction "URI.get* accessors" do
  let u5 : URI := { scheme := "http", authority := some "user%20name@host", path := "/foo%20bar", query := some "q=1%202", fragment := some "frag%20ment" }
  testEq "URI.getAuthority" (u5.getAuthority) (some "user name@host")
  testEq "URI.getPath" (u5.getPath) (some "/foo bar")
  testEq "URI.getQuery" (u5.getQuery) (some "q=1 2")
  testEq "URI.getFragment" (u5.getFragment) (some "frag ment")

def testRelativeRefGetAccessors : TestM Unit := testFunction "RelativeRef.get* accessors" do
  let r2 : RelativeRef := { authority := some "user%20name@host", path := "/foo%20bar", query := some "q=1%202", fragment := some "frag%20ment" }
  testEq "RelativeRef.getAuthority" (r2.getAuthority) (some "user name@host")
  testEq "RelativeRef.getPath" (r2.getPath) (some "/foo bar")
  testEq "RelativeRef.getQuery" (r2.getQuery) (some "q=1 2")
  testEq "RelativeRef.getFragment" (r2.getFragment) (some "frag ment")

def allPublicAPITests : TestM Unit := group "LeanUri.Basic Public API" do
  testURIParse
  testRelativeRefParse
  testParseReference
  testURIToString
  testRelativeRefToString
  testURINormalize
  testURIResolve
  testURIResolveRef
  testURIIsAbsolute
  testURIMkEncoded
  testRelativeRefMkEncoded
  testURIEquivalent
  testURIGetAccessors
  testRelativeRefGetAccessors
