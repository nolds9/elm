module Main exposing (..)

import String


upcase : String -> String
upcase name =
    name
        |> String.length
        |> \len ->
            if len > 10 then
                String.toUpper name
            else
                name
