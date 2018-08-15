port module Main exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (program, div, button, text, br)
import Html.Events exposing (onMouseDown, onMouseUp, onMouseLeave)
import Html.Attributes exposing (style)
import Keyboard as Keyboard
import Set exposing (Set)

main =
  program { init = init, view = view, update = update, subscriptions=subscriptions }
  

-- MODEL

type alias PressedNotes = Set String
  

init = 
  ( { pressedNotes = Set.empty
    , pressedKeys = Set.empty
    }
  , Cmd.none
  )


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
addPressedNote : PressedNotes -> ONote -> PressedNotes
addPressedNote pressedNotes oNote =
  Set.insert (toString oNote) pressedNotes
  
removePressedNote : PressedNotes -> ONote -> PressedNotes
removePressedNote pressedNotes oNote =
  Set.remove (toString oNote) pressedNotes



update msg model =
  case msg of
    PlayNote note octave ->
      ( {model | pressedNotes = addPressedNote model.pressedNotes (ONote note octave)} 
      , playNote (ONote note octave)
      )
    
    StopNote note octave ->
      ( {model | pressedNotes = removePressedNote model.pressedNotes (ONote note octave)}
      , stopNote (ONote note octave)
      )

    KeyDown key ->
      case (oNoteFromKey key) of
        Just oNote -> 
          ( { model | 
              pressedNotes = addPressedNote model.pressedNotes oNote,
              pressedKeys = Set.insert key model.pressedKeys
            }
          , if Set.member key model.pressedKeys
            then Cmd.none
            else playNote oNote
          )
        Nothing -> (model, Cmd.none)

    KeyUp key ->
      case (oNoteFromKey key) of
        Just oNote -> 
          ( { model | 
              pressedNotes = removePressedNote model.pressedNotes oNote,
              pressedKeys = Set.remove key model.pressedKeys
            }
          , stopNote oNote
          )
        Nothing -> (model, Cmd.none)
-- VIEW

view model =
  let 
    handlers note octave = 
      [ onMouseDown (PlayNote note octave)
      , onMouseUp (StopNote note octave)
      , onMouseLeave (StopNote note octave)
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