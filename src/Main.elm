port module Main exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (program, div, button, text, br)
import Html.Events exposing (onClick)


main =
  program { init = model, view = view, update = update, subscriptions=subscriptions }
  

-- MODEL

model = (0, Cmd.none)


-- UPDATE

type Note 
  = C1 
  | C1S 
  | D1 
  | D1S
  | E1 
  | F1
  | F1S
  | G1
  | G1S
  | A1
  | A1S
  | B1

type Msg = Increment | Decrement | PlayNote Note


update msg model =
  case msg of
    PlayNote note ->
      (model, playNote note)

    Increment ->
      (model + 1, Cmd.none)

    Decrement ->
      (model - 1, Cmd.none)
      

-- VIEW

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    
    , br [] []

    , button [ onClick (PlayNote A1) ] [ text "A1" ]
    , button [ onClick (PlayNote C1S) ] [ text "C1s" ]
    ]



-- SUBSCRIPTIONS


-- subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- PORTS

noteToStr : Note -> String
noteToStr note =
  toString note
  |> String.toLower

playNote : Note -> Cmd msg
playNote note =
  noteToStr note
  |> playStringNote

port playStringNote : String -> Cmd msg