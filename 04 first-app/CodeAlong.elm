module Main exposing (..)

import Html.App as App
import Html exposing (Html, text, div, button, h3, input, p)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


-- MODEL


type alias Model =
    { calories : Int
    , input : Int
    , error : Maybe String
    }


initModel : Model
initModel =
    Model 0 0 Nothing



-- UPDATE
-- what actions are possible in our App?


type Msg
    = AddCalories
    | Input String
    | Clear



-- When an action occurs, how should we update our model?


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCalories ->
            { model
                | calories = model.calories + model.input
                , input = 0
            }

        Input val ->
            case String.toInt val of
                Ok input ->
                    { model
                        | input = input
                        , error = Nothing
                    }

                Err err ->
                    { model | error = Just err }

        Clear ->
            initModel



-- VIEW
-- considering the state of the Model, what should be displayed


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ h3 [] [ text ("Total Calories " ++ (toString model.calories)) ]
            , input
                [ type' "text"
                , onInput Input
                , value
                    (if model.input == 0 then
                        ""
                     else
                        toString model.input
                    )
                ]
                []
            , div [] [ text (Maybe.withDefault "" model.error) ]
            ]
        , button [ type' "submit", (onClick AddCalories) ]
            [ text "Add"
            ]
        , button [ type' "submit", onClick Clear ]
            [ text "Clear"
            ]
        , p [] [ text (toString model) ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
