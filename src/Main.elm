port module Main exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (program, div, button, text)
import Html.Events exposing (onClick)


main =
  program { init = model, view = view, update = update, subscriptions=subscriptions }
  

-- MODEL

model = (0, Cmd.none)


-- UPDATE

type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      (model + 1, playNote "a1")

    Decrement ->
      (model - 1, playNote "c1s")
      

-- VIEW

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]



-- SUBSCRIPTIONS


-- subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- PORTS

port playNote : String -> Cmd msg