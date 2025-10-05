import Std.Internal.Parsec.String
import LeanUri

open LeanUri
open Std.Internal.Parsec.String

namespace Testing


inductive TestTree where
  | test (name : String) (passed : Bool) (skipped : Bool := false) (message : Option String := none)
  | group (name : String) (children : Array TestTree) (skipped : Bool := false)
deriving Inhabited, Repr

structure TestState where
  stack : List (String × Array TestTree) := [ ("", #[]) ] -- (group name, children)
  filter : String → Bool := fun _ => true
deriving Inhabited

abbrev TestM := StateT TestState IO

def TestM.run (x : TestM α) (filter : String → Bool := fun _ => true) : IO α :=
  (x { stack := [("", #[])], filter := filter }).map Prod.fst

def recordTest (name : String) (passed : Bool) (msg? : Option String := none) (skipped := false) : TestM Unit := do
  let s ← get
  match s.stack with
  | [] => pure ()
  | (g, children) :: rest =>
      let newChildren := children.push (TestTree.test name passed skipped msg?)
      set { s with stack := (g, newChildren) :: rest }

def test (name : String) (passed : Bool) : TestM Unit := do
  if passed then
    recordTest name true none
  else
    recordTest name false (some "FAILED")
    let s ← get
    let groupNames := s.stack.reverse.map (·.fst)
    let fullName := (String.intercalate "/" (groupNames ++ [name])).trim
    IO.println s!"✗ {fullName} FAILED"

def skip (name : String) (_ : TestM α): TestM Unit :=
  recordTest name true none true

/-- Returns true if all tests (recursively) passed or were skipped. -/
partial def allPass : TestTree → Bool
  | TestTree.test _ passed skipped _ => passed || skipped
  | TestTree.group _ children _  => children.all allPass

def group (name : String) (t : TestM Unit) : TestM Unit := do
  modify fun s => { s with stack := (name, #[]) :: s.stack }
  t
  let s' ← get
  match s'.stack with
  | (g, children) :: (pg, pchildren) :: rest =>
      let newParent := (pg, pchildren.push (TestTree.group g children))
      set { s' with stack := newParent :: rest }
  | _ => pure ()

def testFunction (name : String) (t : TestM Unit) : TestM Unit := do
  let s ← get
  if s.filter name then
    group name t
  else
    match s.stack with
    | (pg, pchildren) :: rest =>
        let newParent := (pg, pchildren.push (TestTree.group name #[] true))
        set { s with stack := newParent :: rest }
    | _ => pure ()

def countTree : TestTree → (Nat × Nat × Nat)
  | TestTree.test _ passed skipped _ =>
      if skipped then (1, 0, 0)
      else if passed then (1, 1, 0) else (1, 0, 1)
  | TestTree.group _ children _ =>
      children.foldl (fun (t, p, f) c =>
        let (ct, cp, cf) := countTree c
        (t + ct, p + cp, f + cf)) (0, 0, 0)

def printTreeSummary (tree : TestTree) (indent : String := "") : IO Unit :=
  match tree with
  | TestTree.test name passed skipped msg? =>
      if !passed && !skipped then
        IO.println s!"{indent}✗ {name}{match msg? with | some m => ": " ++ m | none => ""}"
      else if skipped then
        IO.println s!"{indent}- {name} (skipped)"
      else
        pure ()
  | TestTree.group name children false => do
      let (t, p, f) := countTree (TestTree.group name children)
      for c in children do
        printTreeSummary c (indent ++ "  ")
      IO.println s!"{indent}{name}: {p}/{t} passed, {f} failed."
  | TestTree.group name _ true => do
      IO.println s!"{indent}{name}: Skipped."

def printSummary : TestM Unit := do
  let s ← get
  match s.stack with
  | [(_, children)] =>
      for c in children do
        printTreeSummary c
  | _ => IO.println "[test structure error]"

def testParser {α : Type} [ToString α] (parser : Parser α) (input : String) (expected : String) : Bool :=
  match parser.run input with
  | .ok result => toString result == expected
  | .error _ => false

def expectError {α : Type} (parser : Parser α) (input : String) : Bool :=
  match parser.run input with
  | .ok _ => false
  | .error _ => true

/-- Test parser and ensure all input is consumed -/
def testParserComplete {α : Type} [ToString α] (parser : Parser α) (input : String) (expected : String) : Bool :=
  let fullParser := parser <* Std.Internal.Parsec.eof
  match fullParser.run input with
  | .ok result => toString result == expected
  | .error _ => false

/-- Expect error when parsing complete input -/
def expectErrorComplete {α : Type} (parser : Parser α) (input : String) : Bool :=
  let fullParser := parser <* Std.Internal.Parsec.eof
  match fullParser.run input with
  | .ok _ => false
  | .error _ => true

end Testing
