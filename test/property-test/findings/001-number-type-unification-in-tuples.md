# Bug: Float/Int number type unification in tuples and triples

## Summary

When a tuple or triple contains both Float and Int expressions using arithmetic operators, elm-codegen's type inference incorrectly unifies them, collapsing Float to Int in the generated type annotation.

## Reproduction

Any seed that generates a tuple/triple mixing Float arithmetic and Int arithmetic. Examples:

- `SEED=777 COUNT=20 sh run.sh` — Test8
- `SEED=6 COUNT=15 sh run.sh` — Test14
- `SEED=12 COUNT=15 sh run.sh` — Test16

## Generated code (seed 777, Test8)

```elm
triple810 : ( Int, (), Int )
triple810 =
    ( -978.6500859241711 - -212.06359427069162, (), 408 + 992 )
```

The annotation says `( Int, (), Int )` but the first element is `Float` subtraction. It should be `( Float, (), Int )`.

## Another example (seed 6, Test14)

```elm
tuple1400 : ( Int, Int )
tuple1400 =
    ( -42.82880688295393 + 316.3526900997824, -572 * -907 )
```

Should be `( Float, Int )`.

## Root cause hypothesis

`Elm.Op.plus`, `Elm.Op.minus`, `Elm.Op.multiply` likely infer their result type as `number` (Elm's typeclass for both Int and Float). When both a Float-`number` and an Int-`number` appear in a tuple, the unification in `Internal/Compiler.elm`'s `unifiable` function resolves both `number` constraints to `Int` instead of keeping the Float one as `Float`.

In Elm's actual type system, `5.0 + 3.0` has type `Float` (not `number`), because the float literal narrows the constraint. elm-codegen may not be tracking this narrowing.

## Affected API

- `Elm.tuple` / `Elm.triple` when elements use `Elm.Op.plus/minus/multiply` with `Elm.float` literals
- Likely also affects `Elm.list` with mixed-number operator expressions, and possibly function argument inference

## Elm compiler error

```
-- TYPE MISMATCH

Something is off with the body of the `triple810` definition:

    ( -978.6500859241711 - -212.06359427069162, (), 408 + 992 )
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The body is a tuple of type:

    ( Float, (), Int )

But the type annotation on `triple810` says it should be:

    ( Int, (), Int )
```
