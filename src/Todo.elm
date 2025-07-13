module Todo exposing (Todo, TodoMsg(..), decodeTodos, encodeTodos, update, viewTodo)

import Html exposing (Html, input, li, text)
import Html.Attributes exposing (checked, type_)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Todo =
    { id : Int
    , text : String
    , completed : Bool
    }


type TodoMsg
    = AddTodo
    | ToggleTodo Int
    | DeleteTodo Int
    | TodosLoaded (List Todo)


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


update : TodoMsg -> List Todo -> String -> ( List Todo, Cmd TodoMsg )
update msg todos input =
    case msg of
        AddTodo ->
            let
                newId =
                    case List.maximum (List.map .id todos) of
                        Nothing ->
                            1

                        Just value ->
                            value + 1

                newTodo =
                    { id = newId, text = input, completed = False }
            in
            ( todos ++ [ newTodo ], Cmd.none )

        ToggleTodo tid ->
            let
                toggle todo =
                    if todo.id == tid then
                        { todo | completed = not todo.completed }

                    else
                        todo
            in
            ( List.map toggle todos, Cmd.none )

        DeleteTodo tid ->
            ( List.filter (\todo -> todo.id /= tid) todos, Cmd.none )

        TodosLoaded newTodos ->
            ( newTodos, Cmd.none )


viewTodo : (Int -> TodoMsg) -> (Int -> TodoMsg) -> Todo -> Html TodoMsg
viewTodo toggleMsg deleteMsg todo =
    li []
        [ input
            [ type_ "checkbox"
            , checked todo.completed
            , onClick (toggleMsg todo.id)
            ]
            []
        , text todo.text
        , Html.button [ onClick (deleteMsg todo.id) ] [ text "x" ]
        ]
