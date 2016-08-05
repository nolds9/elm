module Main exposing (..)

import Html exposing (Html, text, div, ul, li, p, h1, h2, h3)
import Html.App as App
import Html.Attributes exposing (..)
import Array exposing (Array, toList, fromList, push)


-- import Html.Events exposing (..)
-- import Debug exposing (log)



-- MODEL


type alias Model =
    { students : Array Student
    , studentName : String
    , currentStudent : Student
    }


type alias Student =
    { id : Int
    , name : String
    }



-- seeds


mary : Student
mary =
    Student 1 "Mary"


jerry : Student
jerry =
    Student 2 "Jerry"


gary : Student
gary =
    Student 3 "Gary"


defaultStudents : Array Student
defaultStudents =
    fromList [ mary, jerry, gary ]


defaultStudent : Student
defaultStudent =
    Student 0 ""


initModel : Model
initModel =
    Model defaultStudents "" mary



-- UPDATE


type Msg
    = AddStudent String



-- | NameInput
-- | SetCurrent Student
-- | RemoveStudent Student
-- | CancelInput


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddStudent name ->
            -- grab the last Student in the Model.students if there is one
            let
                reversedStudents =
                    model
                        |> .students
                        |> toList
                        |> List.reverse
                        |> fromList

                lastStudent =
                    if not (Array.isEmpty model.students) then
                        Array.get 0 reversedStudents
                    else
                        Nothing

                resolveStudentMaybe student =
                    case student of
                        Just student ->
                            student

                        Nothing ->
                            defaultStudent

                resolvedStudent =
                    resolveStudentMaybe lastStudent

                newStudent =
                    Student (resolvedStudent.id + 1) name
            in
                -- add the student to back of the queue
                { model
                    | students =
                        push newStudent model.students
                }



-- VIEW


renderHeader : Html msg
renderHeader =
    h1 [ id "title" ] [ text "Kewy" ]


renderStudent : Student -> Html msg
renderStudent student =
    li [ class "student" ] [ text student.name ]


renderQueue : Model -> Html msg
renderQueue model =
    let
        studentsList =
            model.students
                |> Array.map renderStudent
                |> toList
    in
        div [ class "queue-container" ]
            [ h3 [] [ text "Next" ]
            , ul [ class "students-list " ] (studentsList)
            ]


renderCurrentStudent : Student -> Html msg
renderCurrentStudent student =
    div [ class "current-container" ]
        [ h3 [ id "active" ] [ text "Helping" ]
        , p [ class "student-name" ] [ text student.name ]
        ]


view : Model -> Html msg
view model =
    div [ class "main-container" ]
        [ renderHeader
        , div
            [ class "main-content" ]
            [ (renderQueue model)
            , renderCurrentStudent mary
            ]
        ]



-- MAIN


main : Program Never
main =
    App.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
