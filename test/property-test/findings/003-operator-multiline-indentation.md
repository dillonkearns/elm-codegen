# Bug: Operators with multi-line right operand render at wrong indentation

## Summary

When an infix operator's right-hand operand renders as a multi-line expression (e.g., a function application with a lambda), elm-codegen places the continuation at lower indentation than the operator line. This violates Elm's layout rules and causes a parse error.

## Reproduction

Seeds that generate comparison operators with lambda expressions as operands:

- `SEED=6 COUNT=20 sh run.sh` — Test13
- `SEED=8 COUNT=20 sh run.sh` — Test3

## Generated code (seed 6, Test13)

```elm
value1300 : Bool
value1300 =
    case "hello" of
        "hello" ->
            "wsrz"

        "world" ->
            "p" ++ ""

        _ ->
            "j" ++ "bx" >
        (\lambdaArg -> "") ()
```

The `>` operator's right operand `(\lambdaArg -> "") ()` renders at lower indentation than the `"j" ++ "bx" >` line. Elm requires continuation lines to be indented *more* than the initial line.

## Expected rendering

```elm
        _ ->
            "j" ++ "bx"
                > ((\lambdaArg -> "") ())
```

Or on one line:
```elm
        _ ->
            ("j" ++ "bx") > ((\lambdaArg -> "") ())
```

## Root cause

The pretty-printer in `Internal/Write.elm` renders infix operator expressions as `left <op> right` but doesn't check if the right operand is multi-line. When it is, the right operand's continuation lines end up at the wrong indentation level.

This likely affects any infix operator where the right operand is a complex expression: `>`, `<`, `>=`, `<=`, `==`, `/=`, `++`, `+`, `-`, `*`, etc.

## Elm error

```
ParsingError: src/Test13.elm is not a correct Elm module

I could not understand the content of this file...
```

Or specifically:
```
-- UNEXPECTED SYMBOL

I ran into an unexpected symbol
```

## Workaround

Keep operator operands as simple (single-line) expressions. This is why `operatorExpressionGenerator` uses `literalGenerator` for operands rather than arbitrary expressions.
