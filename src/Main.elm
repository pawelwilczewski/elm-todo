port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (checked, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (decodeString)
import Json.Encode as Encode exposing (Value)


port saveTodos : Encode.Value -> Cmd msg


port loadTodos : (Decode.Value -> msg) -> Sub msg


type alias Model =
    { todos : List Todo
    , input : String
    }


type alias Todo =
    { id : Int
    , text : String
    , completed : Bool
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
    | AddTodo
    | ToggleTodo Int
    | DeleteTodo Int
    | TodosLoaded (List Todo)


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadTodos
        (\value ->
            case Decode.decodeValue decodeTodos value of
                Ok todos ->
                    TodosLoaded todos

                Err _ ->
                    TodosLoaded []
        )


encodeTodo : Todo -> Encode.Value
encodeTodo todo =
    Encode.object
        [ ( "id", Encode.int todo.id )
        , ( "text", Encode.string todo.text )
        , ( "completed", Encode.bool todo.completed )
        ]


encodeTodos : List Todo -> Encode.Value
encodeTodos todos =
    Encode.list encodeTodo todos


decodeTodo : Decode.Decoder Todo
decodeTodo =
    Decode.map3 Todo
        (Decode.field "id" Decode.int)
        (Decode.field "text" Decode.string)
        (Decode.field "completed" Decode.bool)


decodeTodos : Decode.Decoder (List Todo)
decodeTodos =
    Decode.list decodeTodo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput str ->
            ( { model | input = str }, Cmd.none )

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

                newModel =
                    { model
                        | todos = model.todos ++ [ newTodo ]
                        , input = ""
                    }
            in
            ( newModel, saveTodos (encodeTodos newModel.todos) )

        ToggleTodo index ->
            let
                toggle todo =
                    if todo.id == index then
                        { todo | completed = not todo.completed }

                    else
                        todo

                newModel =
                    { model | todos = List.map toggle model.todos }
            in
            ( newModel, saveTodos (encodeTodos newModel.todos) )

        DeleteTodo index ->
            let
                newModel =
                    { model | todos = List.filter (\todo -> todo.id /= index) model.todos }
            in
            ( newModel, saveTodos (encodeTodos newModel.todos) )

        TodosLoaded todos ->
            ( { model | todos = todos }, Cmd.none )


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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
