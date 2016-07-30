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


updateFree : Item -> Int -> Int -> Item
updateFree item qty free =
    if item.qty >= qty then
        { item
            | freeQty = item.freeQty + free
        }
    else
        item


out : List Item -> String
out cart =
    toString (List.map updateFree cart)


main : Html msg
main =
    text (out cart)
