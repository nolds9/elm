module Main exposing (..)

import Html exposing (Html, text)


type alias Item =
    { name : String
    , qty : Int
    , freeQty : Int
    }


cart : List Item
cart =
    [ { name = "Lemon", qty = 1, freeQty = 0 }
    , { name = "Apple", qty = 5, freeQty = 0 }
    , { name = "Pear", qty = 10, freeQty = 0 }
    ]


updateFree : Int -> Int -> Item -> Item
updateFree qty free item =
    if item.qty >= qty then
        { item
            | freeQty = item.freeQty + free
        }
    else
        item


out : List Item -> String
out cart =
    toString (List.map (updateFree 3 5) cart)


main : Html msg
main =
    text (out cart)
