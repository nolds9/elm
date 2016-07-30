module Main exposing (..)

import Html exposing (text)
import String exposing (slice, toLower, length, words)


-- Exercise 1


(~=) : String -> String -> Bool
(~=) frst scnd =
    let
        gfl str =
            slice 0 1 str

        dfl str =
            toLower (gfl str)

        fL =
            gfl frst

        sL =
            gfl scnd
    in
        dfl sL == dfl fL



-- Exercise 2


isSame : String -> String -> Bool
isSame frst scnd =
    let
        gfl str =
            slice 0 1 str

        dfl str =
            toLower (gfl str)

        fL =
            gfl frst

        sL =
            gfl scnd
    in
        dfl sL == dfl fL



-- Exercise 3


wordCount : String -> Int
wordCount str =
    List.length <| words <| str


out : String
out =
    "Hello, what a wonderful day in the city, wouldn't you agree?"
        |> wordCount
        |> toString


main : Html.Html msg
main =
    text out
