module OperatorPrecedence exposing (get, operators, pipes)

import Elm
import Elm.Annotation as Type
import Elm.Arg
import Elm.Case
import Elm.Expect
import Elm.ToString
import Expect
import Elm.Op exposing (equal, multiply, or, plus)
import Test exposing (Test, describe, test)


todo : String -> Elm.Expression
todo todoArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Debug" ]
            , name = "todo"
            , annotation = Just (Type.function [ Type.string ] (Type.var "a"))
            }
        )
        [ Elm.string todoArg ]


valueTodo : Elm.Expression
valueTodo =
    Elm.value
        { importFrom = [ "Debug" ]
        , name = "todo"
        , annotation = Just (Type.function [ Type.string ] (Type.var "a"))
        }


get : Test
get =
    describe "Elm.get"
        [ test "Works correctly if the argument is a function call" <|
            \_ ->
                Elm.get "field" (todo "todo")
                    |> Elm.Expect.renderedAs """(Debug.todo "todo").field"""
        , test "Works correctly if the argument is a value" <|
            \_ ->
                Elm.get "field" valueTodo
                    |> Elm.Expect.renderedAs """Debug.todo.field"""
        , test "Works correctly if the argument is a record" <|
            \_ ->
                Elm.get "field" (Elm.record [])
                    |> Elm.Expect.renderedAs """{}.field"""
        , test "Works correctly if the argument is a record update" <|
            \_ ->
                Elm.get "field" (Elm.updateRecord [ ( "bar", Elm.int 3 ) ] (Elm.val "foo"))
                    |> Elm.Expect.renderedAs """{ foo | bar = 3 }.field"""
        , test "Works correctly if the argument is a field" <|
            \_ ->
                Elm.get "field2" (Elm.get "field1" (Elm.record []))
                    |> Elm.Expect.renderedAs """{}.field1.field2"""
        ]


operators : Test
operators =
    let
        two : Elm.Expression
        two =
            Elm.int 2

        three : Elm.Expression
        three =
            Elm.int 3

        five : Elm.Expression
        five =
            Elm.int 5
    in
    describe "+ and *"
        [ test "(2 + 3) + 5" <|
            \_ ->
                plus (plus two three) five
                    |> Elm.Expect.renderedAs """2 + 3 + 5"""
        , test "(2 * 3) * 5" <|
            \_ ->
                multiply (multiply two three) five
                    |> Elm.Expect.renderedAs """2 * 3 * 5"""
        , test "(2 + 3) * 5" <|
            \_ ->
                multiply (plus two three) five
                    |> Elm.Expect.renderedAs """(2 + 3) * 5"""
        , test "(2 * 3) + 5" <|
            \_ ->
                plus (multiply two three) five
                    |> Elm.Expect.renderedAs """2 * 3 + 5"""
        , test "2 + (3 * 5)" <|
            \_ ->
                plus two (multiply three five)
                    |> Elm.Expect.renderedAs """2 + 3 * 5"""
        , test "2 * (3 + 5)" <|
            \_ ->
                multiply two (plus three five)
                    |> Elm.Expect.renderedAs """2 * (3 + 5)"""
        , test "(2 + 3) + (3 + 5)" <|
            \_ ->
                plus (plus two three) (plus three five)
                    |> Elm.Expect.renderedAs "2 + 3 + (3 + 5)"
        , test "(2 == 3) == (3 == 5)" <|
            \_ ->
                equal (equal two three) (equal three five)
                    |> Elm.Expect.renderedAs "(2 == 3) == (3 == 5)"
        , test "2 || (3 || 5)" <|
            \_ ->
                or two (or three five)
                    |> Elm.Expect.renderedAs "2 || 3 || 5"
        , test "(2 || 3) || 5" <|
            \_ ->
                or (or two three) five
                    |> Elm.Expect.renderedAs "(2 || 3) || 5"
        ]


pipes : Test
pipes =
    describe "Operators in nested contexts"
        [ test "Operator with multi-line right operand wraps in parens" <|
            \_ ->
                -- The bug: when an operator's right operand is a multi-line
                -- expression (like a case expression), the right operand
                -- renders at wrong indentation without parens:
                --     'f' >=
                -- case Just ... of   <-- less indented, parse error!
                --
                -- Fix: wrap multi-line right operands in parens.
                Elm.Op.gte
                    (Elm.char 'f')
                    (Elm.Case.maybe (Elm.just (Elm.bool True))
                        { nothing = Elm.char 'm'
                        , just = ( "val", \_ -> Elm.char 'v' )
                        }
                    )
                    |> Elm.Expect.renderedAs
                        """'f' >= (case Just True of
            Nothing ->
                'm'

            Just val ->
                'v')"""
        , test "Comparison in case branch doesn't break across lines" <|
            \_ ->
                -- Reproduced from property test seed 2, Test13:
                -- a comparison inside a case branch breaks across lines
                -- with the right operand at wrong indentation.
                -- The bug is triggered by the type inference error
                -- (Nothing returns Char, Just returns Bool) which causes
                -- the annotation to be omitted, changing the layout.
                let
                    file =
                        Elm.file [ "Test" ]
                            [ Elm.declaration "value1300"
                                (Elm.Case.maybe (Elm.just (Elm.bool True))
                                    { nothing = Elm.char 'e'
                                    , just =
                                        ( "mV0x123"
                                        , \_ ->
                                            Elm.Op.lt (Elm.char 'j') (Elm.char 'y')
                                        )
                                    }
                                )
                            , Elm.declaration "value1301"
                                (Elm.Op.plus (Elm.int 65) (Elm.int -134))
                            , Elm.alias "Record"
                                (Type.record
                                    [ ( "alpha", Type.char )
                                    , ( "beta", Type.unit )
                                    , ( "gamma", Type.float )
                                    , ( "delta", Type.bool )
                                    ]
                                )
                            , Elm.declaration "record"
                                (Elm.record
                                    [ ( "alpha", Elm.char 'z' )
                                    , ( "beta", Elm.unit )
                                    , ( "gamma", Elm.float 758.45 )
                                    , ( "delta", Elm.bool True )
                                    ]
                                )
                            ]

                    hasOrphanedOperator =
                        file.contents
                            |> String.lines
                            |> List.any
                                (\line ->
                                    let
                                        trimmed =
                                            String.trimRight line
                                    in
                                    String.endsWith " <" trimmed
                                        || String.endsWith " >" trimmed
                                        || String.endsWith " >=" trimmed
                                        || String.endsWith " <=" trimmed
                                )
                in
                if hasOrphanedOperator then
                    Expect.fail
                        ("Operator orphaned at end of line:\n" ++ file.contents)

                else
                    Expect.pass
        ]
