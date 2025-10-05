import LeanUri
open LeanUri

/-- info: "https://example.com/a/c?x#y" -/
#guard_msgs in
#eval match URI.parse "https://EXAMPLE.com/a/./b/../c?x#y" with
| .ok u   => (URI.normalize u).toString
| .error e => s!"error: {e}"

def base : URI := { scheme := "https", authority := some "example.com", path := "/dir/index.html" }
/-- info: Except.ok "https://example.com/x" -/
#guard_msgs in
#eval (URI.resolve base "../x").map (·.toString)
