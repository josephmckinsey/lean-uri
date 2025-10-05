namespace LeanUri

structure URI where
  scheme : String
  authority : Option String
  path : String
  query : Option String
  fragment : Option String
deriving Repr, BEq

structure RelativeRef where
  authority : Option String
  path : String
  query : Option String
  fragment : Option String
  deriving Repr, BEq

def URI.toRelativeRef (uri : URI) : RelativeRef := {
  authority := uri.authority
  path := uri.path
  query := uri.query
  fragment := uri.fragment
}

instance : ToString RelativeRef where
  toString ref :=
    (match ref.authority with
    | some auth => "//" ++ auth
    | none => "") ++
    ref.path ++
    (match ref.query with
    | some q => "?" ++ q
    | none => "") ++
    (match ref.fragment with
    | some f => "#" ++ f
    | none => "")

instance : ToString URI where
  toString uri := uri.scheme ++ ":" ++ toString uri.toRelativeRef

end LeanUri
