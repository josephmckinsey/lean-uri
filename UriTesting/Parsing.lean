import LeanUri
import UriTesting.Helpers


open LeanUri
open Testing
open Testing (TestM TestState test printSummary)

/-! ## Parsing Tests -/

def testPctEncoded : TestM Unit := testFunction "Percent Encodings" do
  test "percent space" (testParser pctEncoded "%20" "%20")
  test "percent A" (testParser pctEncoded "%41" "%41")
  test "lowercase hex" (testParser pctEncoded "%7e" "%7e")
  test "uppercase hex" (testParser pctEncoded "%C3" "%C3")
  test "missing hex digit" (expectError pctEncoded "%2")
  test "invalid hex" (expectError pctEncoded "%GG")

def testScheme : TestM Unit := testFunction "Scheme Parsing" do
  test "http" (testParser scheme "http" "http")
  test "https" (testParser scheme "https" "https")
  test "ftp" (testParser scheme "ftp" "ftp")
  test "scheme with plus" (testParser scheme "svn+ssh" "svn+ssh")
  test "scheme with dot" (testParser scheme "x.y" "x.y")
  test "scheme with dash" (testParser scheme "z-39.50" "z-39.50")
  test "starts with digit" (expectError scheme "9abc")
  test "starts with dash" (expectError scheme "-abc")

def testDecOctet : TestM Unit := testFunction "Dec Octet" do
  test "zero" (testParser decOctet "0" "0")
  test "single digit" (testParser decOctet "5" "5")
  test "two digits" (testParser decOctet "42" "42")
  test "100-199" (testParser decOctet "150" "150")
  test "200-249" (testParser decOctet "200" "200")
  test "250-255" (testParser decOctet "255" "255")
  test "256" (expectError decOctet "256")
  test "300" (expectError decOctet "300")
  test "999" (expectError decOctet "999")

def testIPv4Address : TestM Unit := testFunction "IPv4 Address" do
  test "localhost" (testParser ipv4address "127.0.0.1" "127.0.0.1")
  test "google dns" (testParser ipv4address "8.8.8.8" "8.8.8.8")
  test "private network" (testParser ipv4address "192.168.1.1" "192.168.1.1")
  test "max values" (testParser ipv4address "255.255.255.255" "255.255.255.255")
  test "octet > 255" (expectError ipv4address "192.168.1.256")
  test "missing octet" (expectError ipv4address "192.168.1")

def testIPv6Address : TestM Unit := testFunction "IPv6 Address" do
  test "full form" (testParser ipLiteral "[2001:0db8:85a3:0000:0000:8a2e:0370:7334]" "[2001:0db8:85a3:0000:0000:8a2e:0370:7334]")
  test "loopback" (testParser ipLiteral "[::1]" "[::1]")
  test "all zeros" (testParser ipLiteral "[::]" "[::]")
  test "compressed middle" (testParser ipLiteral "[2001:db8::8a2e:370:7334]" "[2001:db8::8a2e:370:7334]")
  test "compressed start" (testParser ipLiteral "[::ffff:192.0.2.1]" "[::ffff:192.0.2.1]")
  test "compressed end" (testParser ipLiteral "[2001:db8::]" "[2001:db8::]")
  test "IPv4-mapped" (testParser ipLiteral "[::ffff:192.168.1.1]" "[::ffff:192.168.1.1]")
  test "link-local" (testParser ipLiteral "[fe80::1]" "[fe80::1]")
  test "RFC example 1" (testParser ipLiteral "[2001:db8::7]" "[2001:db8::7]")
  test "RFC example 2" (testParser ipLiteral "[::192.0.2.128]" "[::192.0.2.128]")
  test "http with IPv6" (testParser uri "http://[2001:db8::1]/path" "http://[2001:db8::1]/path")
  test "https with IPv6 and port" (testParser uri "https://[::1]:8080/api" "https://[::1]:8080/api")
  test "IPv6 with query" (testParser uri "http://[fe80::1]?key=value" "http://[fe80::1]?key=value")

def testRegName : TestM Unit := testFunction "Reg Name" do
  test "simple domain" (testParser regName "example.com" "example.com")
  test "subdomain" (testParser regName "www.example.com" "www.example.com")
  test "with dash" (testParser regName "my-site.org" "my-site.org")
  test "with tilde" (testParser regName "user~name" "user~name")
  test "percent encoded" (testParser regName "ex%20ample" "ex%20ample")
  test "empty" (testParser regName "" "")

def testPort : TestM Unit := testFunction "Port" do
  test "http port" (testParser port "80" "80")
  test "https port" (testParser port "443" "443")
  test "custom port" (testParser port "8080" "8080")
  test "empty port" (testParser port "" "")
  test "port with letter" (expectErrorComplete port "80a0")
  test "port with dash" (expectErrorComplete port "80-80")

def testHost : TestM Unit := testFunction "Host" do
  test "IPv4 localhost" (testParser host "127.0.0.1" "127.0.0.1")
  test "IPv4 address" (testParser host "192.168.1.1" "192.168.1.1")
  test "domain" (testParser host "example.com" "example.com")
  test "subdomain" (testParser host "www.example.com" "www.example.com")
  test "localhost name" (testParser host "localhost" "localhost")
  test "looks like IPv4 but is reg-name" (testParserComplete host "256.0.0.1" "256.0.0.1")
  test "host with space" (expectErrorComplete host "exam ple.com")
  test "host with @" (expectErrorComplete host "ex@mple.com")

def testAuthority : TestM Unit := testFunction "Authority" do
  test "host only" (testParser authority "example.com" "example.com")
  test "IPv4:port" (testParser authority "127.0.0.1:8080" "127.0.0.1:8080")
  test "user@host" (testParser authority "user@example.com" "user@example.com")
  test "user:pass@host" (testParser authority "user:pass@example.com" "user:pass@example.com")
  test "user@host:port" (testParser authority "admin@example.com:8080" "admin@example.com:8080")
  test "user:pass@host:port" (testParser authority "user:pass@localhost:3000" "user:pass@localhost:3000")
  test "multiple @" (expectErrorComplete authority "user@pass@host")
  test "looks like IPv4 in authority" (testParserComplete authority "user@256.0.0.1:80" "user@256.0.0.1:80")
  test "port with letters" (expectErrorComplete authority "example.com:80a")

def testPath : TestM Unit := testFunction "Path" do
  test "empty path" (testParserComplete pathAbempty "" "")
  test "single segment" (testParserComplete pathAbempty "/foo" "/foo")
  test "multiple segments" (testParserComplete pathAbempty "/foo/bar" "/foo/bar")
  test "with percent-encoding" (testParserComplete pathAbempty "/foo%20bar/baz" "/foo%20bar/baz")
  test "with special chars" (testParserComplete pathAbempty "/foo:bar/@test" "/foo:bar/@test")
  test "absolute root" (testParserComplete pathAbsolute "/" "/")
  test "absolute path" (testParserComplete pathAbsolute "/foo/bar" "/foo/bar")
  test "absolute with colon" (testParserComplete pathAbsolute "/foo:bar" "/foo:bar")
  test "rootless single" (testParserComplete pathRootless "foo" "foo")
  test "rootless multiple" (testParserComplete pathRootless "foo/bar" "foo/bar")
  test "rootless with colon" (testParserComplete pathRootless "foo:bar/baz" "foo:bar/baz")
  test "noscheme simple" (testParserComplete pathNoscheme "foo" "foo")
  test "noscheme multiple" (testParserComplete pathNoscheme "foo/bar" "foo/bar")
  test "noscheme colon in second" (testParserComplete pathNoscheme "foo/bar:baz" "foo/bar:baz")
  test "noscheme with colon in first" (expectErrorComplete pathNoscheme "foo:bar")
  test "path with space" (expectErrorComplete pathAbsolute "/foo bar")
  test "path with hash" (expectErrorComplete pathAbsolute "/foo#bar")
  test "path with question" (expectErrorComplete pathAbsolute "/foo?bar")
  test "path with bracket" (expectErrorComplete pathAbsolute "/foo[bar]")

def testQuery : TestM Unit := testFunction "Query" do
  test "empty query" (testParserComplete query "" "")
  test "simple query" (testParserComplete query "name=value" "name=value")
  test "multiple params" (testParserComplete query "a=1&b=2" "a=1&b=2")
  test "with slash" (testParserComplete query "path/to/resource" "path/to/resource")
  test "with question mark" (testParserComplete query "a=1?b=2" "a=1?b=2")
  test "percent-encoded" (testParserComplete query "name=hello%20world" "name=hello%20world")
  test "with colon" (testParserComplete query "key:value" "key:value")
  test "with @" (testParserComplete query "user@domain" "user@domain")
  test "query with space" (expectErrorComplete query "name=hello world")
  test "query with hash" (expectErrorComplete query "name=value#section")
  test "query with bracket" (expectErrorComplete query "arr[0]=value")

def testFragment : TestM Unit := testFunction "Fragment" do
  test "empty fragment" (testParserComplete fragment "" "")
  test "simple fragment" (testParserComplete fragment "section1" "section1")
  test "with slash" (testParserComplete fragment "section/subsection" "section/subsection")
  test "with question mark" (testParserComplete fragment "section?param=value" "section?param=value")
  test "percent-encoded" (testParserComplete fragment "sec%20tion" "sec%20tion")
  test "with colon" (testParserComplete fragment "line:42" "line:42")
  test "with @" (testParserComplete fragment "user@note" "user@note")
  test "fragment with space" (expectErrorComplete fragment "sec tion")
  test "fragment with hash" (expectErrorComplete fragment "section#subsection")
  test "fragment with bracket" (expectErrorComplete fragment "item[0]")

def testURI : TestM Unit := testFunction "URI" do
  test "http URL" (testParserComplete uri "http://example.com" "http://example.com")
  test "https with path" (testParserComplete uri "https://example.com/path" "https://example.com/path")
  test "with port" (testParserComplete uri "http://example.com:8080" "http://example.com:8080")
  test "with query" (testParserComplete uri "http://example.com?key=value" "http://example.com?key=value")
  test "with path and query" (testParserComplete uri "http://example.com/path?key=value" "http://example.com/path?key=value")
  test "with fragment" (testParserComplete uri "http://example.com#section" "http://example.com#section")
  test "with query and fragment" (testParserComplete uri "http://example.com?key=value#section" "http://example.com?key=value#section")
  test "with userinfo" (testParserComplete uri "http://user@example.com/path" "http://user@example.com/path")
  test "with user:pass" (testParserComplete uri "ftp://user:pass@ftp.example.com/file.txt" "ftp://user:pass@ftp.example.com/file.txt")
  test "full authority" (testParserComplete uri "http://admin:secret@example.com:8080/admin" "http://admin:secret@example.com:8080/admin")
  test "complex URI" (testParserComplete uri "https://user@example.com:443/path/to/resource?query=param&foo=bar#section1" "https://user@example.com:443/path/to/resource?query=param&foo=bar#section1")
  test "IPv4 host" (testParserComplete uri "http://192.168.1.1/path" "http://192.168.1.1/path")
  test "localhost IP" (testParserComplete uri "http://127.0.0.1:8080" "http://127.0.0.1:8080")
  test "mailto" (testParserComplete uri "mailto:user@example.com" "mailto:user@example.com")
  test "file scheme" (testParserComplete uri "file:/path/to/file" "file:/path/to/file")
  test "urn" (testParserComplete uri "urn:isbn:0451450523" "urn:isbn:0451450523")
  test "empty path" (testParserComplete uri "http://example.com" "http://example.com")
  test "root path" (testParserComplete uri "http://example.com/" "http://example.com/")
  test "percent-encoded in path" (testParserComplete uri "http://example.com/hello%20world" "http://example.com/hello%20world")
  test "missing colon" (expectErrorComplete uri "http//example.com")
  test "invalid scheme start" (expectErrorComplete uri "9http://example.com")
  test "space in host" (expectErrorComplete uri "http://exam ple.com")
  test "space in path" (expectErrorComplete uri "http://example.com/hello world")
  test "invalid port" (expectErrorComplete uri "http://example.com:80a/path")
  test "multiple @" (expectErrorComplete uri "http://user@pass@host/path")
