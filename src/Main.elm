module Main exposing (main)

import Browser
import Html exposing (Html, text)

type alias Model = ()

init : Model
init = ()

type Msg = NoOp

update : Msg -> Model -> Model
update msg model = model

view : Model -> Html Msg
view model =
    text "Hello, Elm!"

main =
    Browser.sandbox { init = init, update = update, view = view }
