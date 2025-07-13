port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text, ul)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import Todo exposing (Todo, TodoMsg(..), decodeTodos, encodeTodos)


port saveTodos : Encode.Value -> Cmd msg


port loadTodos : (Decode.Value -> msg) -> Sub msg


type alias Model =
    { todos : List Todo
    , input : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todos = []
      , input = ""
      }
    , Cmd.none
    )


type Msg
    = UpdateInput String
    | TodoMsg TodoMsg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map TodoMsg <|
        loadTodos
            (\value ->
                case Decode.decodeValue decodeTodos value of
                    Ok todos ->
                        TodosLoaded todos

                    Err _ ->
                        TodosLoaded []
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput str ->
            ( { model | input = str }, Cmd.none )

        TodoMsg todoMsg ->
            let
                ( newTodos, todoCmd ) =
                    Todo.update todoMsg model.todos model.input
            in
            ( { model | todos = newTodos, input = "" }
            , Cmd.batch [ saveTodos (encodeTodos newTodos), Cmd.map TodoMsg todoCmd ]
            )


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "What needs to be done?"
            , value model.input
            , onInput UpdateInput
            ]
            []
        , button [ onClick (TodoMsg AddTodo) ] [ text "Add" ]
        , Html.map TodoMsg <| ul [] (List.map (Todo.viewTodo ToggleTodo DeleteTodo) model.todos)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
