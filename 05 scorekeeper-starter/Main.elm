module Main exposing (..)

import Html exposing (Html, img, text, div, ul, li, p, h1, h2, h3, input, button, span)
import Html.App as App
import Html.Attributes exposing (..)
import Array exposing (Array, toList, fromList, push)
import Html.Events exposing (..)


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


larry : Student
larry =
    Student 4 "Larry"


defaultStudents : Array Student
defaultStudents =
    fromList [ jerry, gary, larry ]


defaultStudent : Student
defaultStudent =
    Student 0 ""


initModel : Model
initModel =
    Model defaultStudents "" mary



-- UPDATE


type Msg
    = AddStudent
    | Input String
    | SetCurrent
    | DeleteStudent Student



-- | CancelInput


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddStudent ->
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

                resolvedStudent =
                    resolveStudentMaybe lastStudent

                newStudent =
                    Student (resolvedStudent.id + 1) model.studentName

                notEmpty student =
                    student.name /= ""

                updatedStudents =
                    model.students
                        |> push newStudent
                        |> Array.filter notEmpty
            in
                if ((Array.isEmpty model.students) && model.currentStudent.name == "") then
                    { model
                        | currentStudent = newStudent
                        , studentName = ""
                    }
                else
                    { model
                        | students = updatedStudents
                        , studentName = ""
                    }

        Input str ->
            { model | studentName = str }

        SetCurrent ->
            let
                firstStudent =
                    resolveStudentMaybe (getFirstStudent model.students)

                isNotFirstStudent student =
                    student.id /= firstStudent.id

                updatedStudents =
                    Array.filter isNotFirstStudent model.students
            in
                { model | currentStudent = firstStudent, students = updatedStudents }

        DeleteStudent student ->
            let
                notStudent std =
                    student.id /= std.id

                updatedStudents =
                    Array.filter notStudent model.students
            in
                { model
                    | students = updatedStudents
                }



-- update helpers


resolveStudentMaybe : Maybe Student -> Student
resolveStudentMaybe student =
    case student of
        Just student ->
            student

        Nothing ->
            defaultStudent


getFirstStudent : Array Student -> Maybe Student
getFirstStudent students =
    Array.get 0 students



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main-container" ]
        [ renderHeader
        , div
            [ class "main-content" ]
            [ (renderQueue model)
            , renderCurrentStudent model
            ]
        , (renderNewStudent model)
        ]



--  view helpers


renderHeader : Html Msg
renderHeader =
    img [ id "logo", src "gaq-logo.png" ] []


renderStudent : Student -> Html Msg
renderStudent student =
    li [ class "student" ]
        [ span [ id "delete", onClick (DeleteStudent student) ] [ text "❌" ]
        , text student.name
        ]


renderQueue : Model -> Html Msg
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


renderCurrentStudent : Model -> Html Msg
renderCurrentStudent model =
    if model.currentStudent.name /= "" then
        div [ class "current-container" ]
            [ h3 [ id "active" ] [ text "Helping" ]
            , p [ class "student-name" ]
                [ text model.currentStudent.name
                , span [ id "next", onClick SetCurrent ] [ text "✅" ]
                ]
            ]
    else
        div [ class "current-container" ]
            [ h3 [ id "active" ] [ text "Helping" ] ]


renderNewStudent : Model -> Html Msg
renderNewStudent model =
    div [ class "form-container" ]
        [ Html.form [ onSubmit AddStudent ]
            [ input
                [ id "student-input"
                , type' "text"
                , onInput Input
                , value model.studentName
                , placeholder "Name"
                ]
                []
            , button [ id "student-submit" ] [ text "Add" ]
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
