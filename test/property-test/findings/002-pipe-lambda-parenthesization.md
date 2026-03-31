# Bug: Pipe operator with lambda not parenthesized

## Summary

When `Elm.Op.pipe` is used with a lambda expression (`Elm.fn`) on the right side and the lambda body spans multiple lines, elm-codegen does not wrap the lambda in parentheses. This causes Elm's parser to reject the code.

## Reproduction

Generate a pipe chain where the lambda body is a compound expression (case, let, if-then-else):

```elm
Elm.Op.pipe
    (Elm.string "hello")
    (Elm.fn (Elm.Arg.var "x") (\_ ->
        Elm.Case.maybe (Elm.just (Elm.int 1))
            { nothing = Elm.string "none"
            , just = ("v", \_ -> Elm.string "some")
            }
    ))
```

## Generated code (example)

```elm
piped0 =
    "hello"
        |> \x ->
            case Just 1 of
                Nothing ->
                    "none"

                Just v ->
                    "some"
        |> \y ->
            "done"
```

## Expected code

```elm
piped0 =
    "hello"
        |> (\x ->
                case Just 1 of
                    Nothing ->
                        "none"

                    Just v ->
                        "some"
           )
        |> (\y -> "done")
```

## Root cause

`Elm.Op.pipe` generates the `|>` operator but does not check whether the right-hand operand is a lambda expression that needs parenthesization. In Elm, `a |> \x -> b |> \y -> c` is ambiguous — the second `|>` could be part of the first lambda's body or a new pipe stage.

The fix would be in `Internal/Write.elm` or wherever `|>` expressions are rendered — if the right operand is a `LambdaExpression`, wrap it in parens.

## Elm compiler error

```
-- UNEXPECTED SYMBOL

I ran into an unexpected symbol:

    |> \pipeA ->
    ^^
I was not expecting to see a |> here.
```

## Workaround

Keep lambda bodies in pipe chains as simple leaf expressions (literals, operators) to avoid multi-line rendering. This is what the property test generator currently does.
