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
            { model | name = player.name, playerId = Just player.id }

        Score player points ->
            let
                updatedPlayers =
                    List.map
                        (\plyr ->
                            if plyr.id == player.id then
                                { plyr | points = plyr.points + points }
                            else
                                plyr
                        )
                        model.players

                newPlay =
                    Play (List.length model.plays + 1) points player.id player.name
            in
                { model | players = updatedPlayers, plays = newPlay :: model.plays }

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
            let
                updatedPlayers =
                    List.map
                        (\player ->
                            if player.id == play.playerId then
                                { player | points = (player.points - play.points) }
                            else
                                player
                        )
                        model.players

                updatedPlays =
                    List.filter
                        (\ply ->
                            ply.playerId /= play.playerId
                        )
                        model.plays
            in
                { model | plays = updatedPlays, players = updatedPlayers }


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
        , renderPlayersSection model
        , renderPlayerForm model
        , renderPlaysSection model
        ]


renderPlaysSection : Model -> Html Msg
renderPlaysSection model =
    div []
        [ renderListHeader "Plays"
        , renderPlaysList model
        ]


renderPlaysList : Model -> Html Msg
renderPlaysList model =
    model.plays
        |> List.map renderPlayDetail
        |> ul []


renderPlayDetail : Play -> Html Msg
renderPlayDetail play =
    li []
        [ i [ class "remove", onClick (DeletePlay play) ] []
        , div [] [ text play.name ]
        , div [] [ text (toString play.points) ]
        ]


renderPlayersSection : Model -> Html Msg
renderPlayersSection model =
    div []
        [ renderListHeader "Name"
        , renderPlayerList model
        , renderPointTotal model
        ]


renderListHeader : String -> Html Msg
renderListHeader title =
    header []
        [ div [] [ text title ]
        , div [] [ text "Points" ]
        ]


renderPointTotal : Model -> Html Msg
renderPointTotal model =
    let
        total =
            model.plays
                |> List.map .points
                |> List.sum
                |> toString
    in
        footer []
            [ div [] [ text "Total: " ]
            , div [] [ text total ]
            ]


renderPlayerList : Model -> Html Msg
renderPlayerList model =
    model.players
        |> List.sortBy .name
        |> List.map renderPlayerDetail
        |> ul []


renderPlayerDetail : Player -> Html Msg
renderPlayerDetail player =
    li []
        [ i [ class "edit", onClick (Edit player) ] []
        , div [] [ text player.name ]
        , button [ type' "button", onClick (Score player 2) ] [ text "2pt" ]
        , button [ type' "button", onClick (Score player 3) ] [ text "3pt" ]
        , div [] [ text (toString player.points) ]
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
