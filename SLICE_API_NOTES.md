# Lean 4.27 String.Slice API Notes

## Documentation Sources
- Reference manual source: `/home/joseph/Programming/lean/reference-manual/Manual/BasicTypes/String/Slice.lean`
- Online docs: https://lean-lang.org/doc/reference/latest/Basic-Types/Strings/#string-api-slice

## Key String.Slice API

### Character Access
- `front` / `front?` - first character only
- `back` / `back?` - last character only
- No direct index access like `s[n]`

### Position-Based Access (the right way)
- `String.Slice.pos` / `pos?` - get a `String.Slice.Pos` at index n
- `String.Slice.Pos.get` / `get?` - get character at a position
- `String.Slice.Pos.next` / `nextn` - advance position
- Positions retain reference to their slice

### Byte Access (for ASCII-only like percent-encoding)
- `getUTF8Byte` / `getUTF8Byte!` - direct byte access

### Slicing Operations (return String.Slice, not String)
- `drop`, `take`, `dropWhile`, `takeWhile`
- `dropEnd`, `dropEndWhile` (replaces deprecated `dropRight`, `dropRightWhile`)

### Conversions
- `String.toSlice` - String to Slice
- `String.Slice.toString` or `.copy` - Slice to String

## The Build Errors We're Fixing
In `LeanUri/URI.lean` and `LeanUri/Normalization.lean`:
1. `String.drop` now returns `String.Slice` not `String`
2. `dropRightWhile` deprecated -> use `dropEndWhile`
3. `dropRight` deprecated -> use `dropEnd`
4. `String.Slice.length` deprecated -> use `positions.count`

## Idiomatic Pattern

Lean coerces Strings to String.Slice

```lean
def foo (input : String.Slice) : String :=
```
