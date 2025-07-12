module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (checked, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { todos : List Todo
    , input : String
    }


type alias Todo =
    { id : Int
    , text : String
    , completed : Bool
    }


init : Model
init =
    { todos = []
    , input = ""
    }


type Msg
    = UpdateInput String
    | AddTodo
    | ToggleTodo Int
    | DeleteTodo Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateInput str ->
            { model | input = str }

        AddTodo ->
            let
                newId =
                    case List.maximum (List.map .id model.todos) of
                        Nothing ->
                            1

                        Just value ->
                            value + 1

                newTodo =
                    { id = newId
                    , text = model.input
                    , completed = False
                    }
            in
            { model
                | todos = model.todos ++ [ newTodo ]
                , input = ""
            }

        ToggleTodo index ->
            let
                toggle todo =
                    if todo.id == index then
                        { todo | completed = not todo.completed }

                    else
                        todo
            in
            { model | todos = List.map toggle model.todos }

        DeleteTodo index ->
            { model | todos = List.filter (\todo -> todo.id /= index) model.todos }


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "What needs to be done?"
            , value model.input
            , onInput UpdateInput
            ]
            []
        , button [ onClick AddTodo ] [ text "Add" ]
        , ul [] (List.map viewTodo model.todos)
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    li []
        [ input
            [ type_ "checkbox"
            , checked todo.completed
            , onClick (ToggleTodo todo.id)
            ]
            []
        , text todo.text
        , button [ onClick (DeleteTodo todo.id) ] [ text "x" ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
