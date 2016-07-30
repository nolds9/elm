module Main exposing (..)

import Html exposing (text)


(~+) a b =
    a + b + 0.1


result =
    0 ~+ 2


main =
    result
        |> toString
        |> text
