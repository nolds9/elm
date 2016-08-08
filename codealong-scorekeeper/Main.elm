module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import String


-- Model


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , points : Int
    , playerId : Int
    , name : String
    }


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }



-- Update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Edit player ->
            model

        Score player points ->
            model

        Input str ->
            { model | name = str }

        Save ->
            if (String.isEmpty model.name) then
                model
            else
                save model

        Cancel ->
            { model | name = "", playerId = Nothing }

        DeletePlay play ->
            model


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


add : Model -> Model
add model =
    let
        newPlayer =
            Player (List.length model.players + 1) model.name 0

        newPlayers =
            newPlayer :: model.players
    in
        { model
            | players = newPlayers
            , name = ""
            , playerId = Nothing
        }


edit : Model -> Int -> Model
edit model pid =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == pid then
                        { player | name = model.name }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == pid then
                        { play | name = model.name }
                    else
                        play
                )
                model.plays
    in
        { model
            | plays = newPlays
            , players = newPlayers
            , name = ""
            , playerId = Nothing
        }



-- View


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , renderPlayerForm model
        , p [] [ text (toString model) ]
        ]


renderPlayerForm : Model -> Html Msg
renderPlayerForm model =
    div []
        [ Html.form [ onSubmit Save ]
            [ input
                [ onInput Input
                , type' "text"
                , placeholder "Add/Edit player"
                , value model.name
                ]
                []
            , button [ type' "submit" ] [ text "Save" ]
            , button [ type' "button", onClick Cancel ] [ text "Cancel" ]
            ]
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
