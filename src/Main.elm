port module Main exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (program, div, button, text, br)
import Html.Events exposing (onMouseDown, onMouseUp)
import Html.Attributes exposing (style)
import Keyboard as Keyboard
import Set exposing (Set)

main =
  program { init = model, view = view, update = update, subscriptions=subscriptions }
  

-- MODEL

type alias PressedNotes = Set ONote
  -- { a: Bool
  -- , s: Bool
  -- , d: Bool
  -- , f: Bool
  -- , g: Bool
  -- , h: Bool
  -- , j: Bool
  -- , w: Bool
  -- , e: Bool
  -- , t: Bool
  -- , y: Bool
  -- , u: Bool
  -- }

initialKeyStates = Set.empty
  -- { a = False
  -- , s = False
  -- , d = False
  -- , f = False
  -- , g = False
  -- , h = False
  -- , j = False
  -- , w = False
  -- , e = False
  -- , t = False
  -- , y = False
  -- , u = False
  -- }

model = (initialKeyStates, Cmd.none)


-- UPDATE

type Note 
  = C 
  | CS 
  | D 
  | DS
  | E 
  | F
  | FS
  | G
  | GS
  | A
  | AS
  | B

type alias ONote = { note: Note, octave: Int }

type Msg 
  = PlayNote Note Int 
  | StopNote Note Int 
  | KeyDown Int
  | KeyUp Int


oNoteFromKey : Int -> Maybe ONote
oNoteFromKey key =
  case key of
    65 -> Just (ONote C 1)
    83 -> Just (ONote D 1)
    68 -> Just (ONote E 1)
    70 -> Just (ONote F 1)
    71 -> Just (ONote G 1)
    72 -> Just (ONote A 1)
    74 -> Just (ONote B 1)
    
    87 -> Just (ONote CS 1)
    69 -> Just (ONote DS 1)
    84 -> Just (ONote FS 1)
    89 -> Just (ONote GS 1)
    85 -> Just (ONote AS 1)
    
    x -> Debug.log (toString x) Nothing


-- addPressedNote : ONote -> Set ONote -> Set ONote
-- addPressedNote oNote pressedNotes =
--   Set.insert oNote pressedNotes
  


-- updateKeyState : Int -> Bool -> PressedKeys -> PressedKeys
-- updateKeyState key value keyStates =
--   case key of
--     65 -> { keyStates | a = value }
--     83 -> { keyStates | s = value }
--     68 -> { keyStates | d = value }
--     70 -> { keyStates | f = value }
--     71 -> { keyStates | g = value }
--     72 -> { keyStates | h = value }
--     74 -> { keyStates | j = value }

--     87 -> { keyStates | w = value }
--     69 -> { keyStates | e = value }
--     84 -> { keyStates | t = value }
--     89 -> { keyStates | y = value }
--     85 -> { keyStates | u = value }
    
--     _ -> keyStates
--     -- x -> Debug.log (toString x) Nothing

update msg model =
  let 
    (newModel, cmd) = case msg of
      PlayNote note octave ->
        (model, playNote (ONote note octave))
      
      StopNote note octave ->
        (model, stopNote (ONote note octave))

      _ -> (model, Cmd.none)
      -- KeyDown key ->
      --   ( case (oNoteFromKey key) of 
      --     Just oNote -> addPressedNote oNote
      --     Nothing -> model
      --   , Cmd.none
      --   )
        -- , case (oNoteFromKey key) of 
        --   Just oNote -> playNote oNote
        --   Nothing -> Cmd.none
        -- )

      -- KeyUp key ->
      --   ( updateKeyState key False model
      --   , Cmd.none
      --   )
  in
    (newModel, cmd)
    -- newModel ! (
    --   (List.map playNote
    --     (Set.toList newModel)
    --   ) ++ cmd)
-- VIEW

view model =
  let 
    handlers note octave = 
      [ onMouseDown (PlayNote note octave)
      , onMouseUp (StopNote note octave)
      ]
  in
  div []
    [ div [ style [("padding-left", "15px")] ]
      [ button (handlers CS 1) [ text "C1s" ]
      , button (handlers DS 1) [ text "D1s" ]

      -- spacer lol
      , button [ style [("opacity", "0")] ] [ text "---" ]

      , button (handlers FS 1) [ text "F1s" ]
      , button (handlers GS 1) [ text "G1s" ]
      , button (handlers AS 1) [ text "A1s" ]
      ]
    , div []
      [ button (handlers C 1) [ text "C1-" ]
      , button (handlers D 1) [ text "D1-" ]
      , button (handlers E 1) [ text "E1-" ]
      , button (handlers F 1) [ text "F1-" ]
      , button (handlers G 1) [ text "G1-" ]
      , button (handlers A 1) [ text "A1-" ]
      , button (handlers B 1) [ text "B1-" ]
      ]
    , br [] []
    , div [] [ text (toString model)]
    ]



-- SUBSCRIPTIONS


-- subscriptions : Model -> Sub Msg
-- subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs KeyDown
    , Keyboard.ups KeyUp
    ]

-- PORTS

oNoteToStr : ONote -> String
oNoteToStr oNote =
  toString oNote.note
  |> String.toLower
  |> flip (++) (toString oNote.octave)

playNote : ONote -> Cmd msg
playNote oNote =
  oNoteToStr oNote
  |> playStringNote

stopNote : ONote -> Cmd msg
stopNote oNote =
  oNoteToStr oNote
  |> stopStringNote

port playStringNote : String -> Cmd msg
port stopStringNote : String -> Cmd msg