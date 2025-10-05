/-
Resolution tests for LeanUri
-/


import LeanUri
import UriTesting.Helpers

open LeanUri
open Testing

/-! ## Resolution Tests -/

-- Helper to parse base URI for resolution tests
def parseBaseURI (baseStr : String) : IO (Option URI) := do
  match uri.run baseStr with
  | .ok u => pure (some u)
  | .error e =>
    IO.println s!"✗ Failed to parse base URI: {e}"
    pure none

-- Helper to test URI reference resolution
def testResolve (base : URI) (refStr : String) (expected : String) : Bool :=
  match parseAndResolve base refStr with
  | .ok result =>
    let resultStr := toString result
    resultStr == expected
  | .error _ => false

def testRelativeResolution : TestM Unit := group "Relative Resolution" do
  -- Base URI for all tests (from RFC 3986 Section 5.4)
  let some base ← parseBaseURI "http://a/b/c/d;p?q" | return

  testFunction "Normal Examples (RFC 3986 Section 5.4.1)" do
    let cases := #[
      ("different scheme",         "g:h",         "g:h"),
      ("simple relative",          "g",           "http://a/b/c/g"),
      ("current directory",        "./g",         "http://a/b/c/g"),
      ("relative directory",       "g/",          "http://a/b/c/g/"),
      ("absolute path",            "/g",          "http://a/g"),
      ("network-path",             "//g",         "http://g"),
      ("query replacement",        "?y",          "http://a/b/c/d;p?y"),
      ("relative with query",      "g?y",         "http://a/b/c/g?y"),
      ("fragment only",            "#s",          "http://a/b/c/d;p?q#s"),
      ("relative with fragment",   "g#s",         "http://a/b/c/g#s"),
      ("query and fragment",       "g?y#s",       "http://a/b/c/g?y#s"),
      ("semicolon segment",        ";x",          "http://a/b/c/;x"),
      ("relative with semicolon",  "g;x",         "http://a/b/c/g;x"),
      ("semicolon query fragment", "g;x?y#s",     "http://a/b/c/g;x?y#s"),
      ("empty reference",          "",            "http://a/b/c/d;p?q"),
      ("current dir only",         ".",           "http://a/b/c/"),
      ("current dir with slash",   "./",          "http://a/b/c/"),
      ("parent dir",               "..",          "http://a/b/"),
      ("parent dir with slash",    "../",         "http://a/b/"),
      ("parent then relative",     "../g",        "http://a/b/g"),
      ("two parents",              "../..",       "http://a/"),
      ("two parents with slash",   "../../",      "http://a/"),
      ("two parents then relative","../../g",     "http://a/g")
    ]
    for (name, ref, expected) in cases do
      test name (testResolve base ref expected)

  testFunction "Abnormal Examples (RFC 3986 Section 5.4.2)" do
    let cases := #[
      ("three parents",        "../../../g",      "http://a/g"),
      ("four parents",         "../../../../g",   "http://a/g"),
      ("abs with current",     "/./g",            "http://a/g"),
      ("abs with parent",      "/../g",           "http://a/g"),
      ("dot suffix",           "g.",              "http://a/b/c/g."),
      ("dot prefix",           ".g",              "http://a/b/c/.g"),
      ("dotdot suffix",        "g..",             "http://a/b/c/g.."),
      ("dotdot prefix",        "..g",             "http://a/b/c/..g"),
      ("unnecessary dots",     "./../g",          "http://a/b/g"),
      ("relative with trailing dot", "./g/.",     "http://a/b/c/g/"),
      ("mid-path current dir", "g/./h",           "http://a/b/c/g/h"),
      ("mid-path parent dir",  "g/../h",          "http://a/b/c/h"),
      ("semicolon with dots 1","g;x=1/./y",       "http://a/b/c/g;x=1/y"),
      ("semicolon with dots 2","g;x=1/../y",      "http://a/b/c/y"),
      ("query with dots 1",    "g?y/./x",         "http://a/b/c/g?y/./x"),
      ("query with dots 2",    "g?y/../x",        "http://a/b/c/g?y/../x"),
      ("fragment with dots 1", "g#s/./x",         "http://a/b/c/g#s/./x"),
      ("fragment with dots 2", "g#s/../x",        "http://a/b/c/g#s/../x"),
      ("same scheme colon",    "http:g",          "http:g")
    ]
    for (name, ref, expected) in cases do
      test name (testResolve base ref expected)
