port module Main exposing (..)

-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/user_input/buttons.html

import Html exposing (program, div, button, text, br)
import Keyboard as Keyboard
import Set exposing (Set)
import Dict exposing (Dict)
import List as List
import Time
import Piano
import Task

main : Program Never Model Msg
main =
  program { init = init, view = view, update = update, subscriptions=subscriptions }
  

-- MODEL

type alias PressedNotes = Dict String (Time.Time, ONote)

type alias PressedKeys = Set Int

type alias NoteHistoryItem = 
  { startTime: Time.Time
  , endTime: Time.Time
  , oNote: ONote
  }
type alias Model = 
  { pressedNotes: PressedNotes
  , pressedKeys: PressedKeys
  , noteHistory: List NoteHistoryItem
  }

init : ( Model, Cmd msg )
init = 
  ( { pressedNotes = Dict.empty
    , pressedKeys = Set.empty
    , noteHistory = []
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
  = KeyDown Int
  | KeyUp Int
  | PianoMsg Piano.Msg
  | SetPress Time.Time ONote
  | StoreNote Time.Time ONote


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
    75 -> Just (ONote C 2)
    
    87 -> Just (ONote CS 1)
    69 -> Just (ONote DS 1)
    84 -> Just (ONote FS 1)
    89 -> Just (ONote GS 1)
    85 -> Just (ONote AS 1)
    
    x -> Debug.log (toString x) Nothing


-- addPressedNote : ONote -> Set ONote -> Set ONote
addPressedNote : PressedNotes -> Time.Time -> ONote -> PressedNotes
addPressedNote pressedNotes time oNote =
  Dict.insert (toString oNote) (time, oNote) pressedNotes
  
removePressedNote : PressedNotes -> ONote -> PressedNotes
removePressedNote pressedNotes oNote =
  Dict.remove (toString oNote) pressedNotes

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    KeyDown key ->
      case (oNoteFromKey key) of
        Just oNote -> 
          ( { model | pressedKeys = Set.insert key model.pressedKeys }
          , if Set.member key model.pressedKeys
            then Cmd.none 
            else Cmd.batch 
              [ playNote oNote
              , Task.perform (flip SetPress oNote) Time.now
              ]
          )
        Nothing -> (model, Cmd.none)

    KeyUp key ->
      case (oNoteFromKey key) of
        Just oNote -> 
          ( { model | pressedKeys = Set.remove key model.pressedKeys }
          , Cmd.batch
            [ stopNote oNote
            , Task.perform (flip StoreNote oNote) Time.now
            ]
          )
        Nothing -> (model, Cmd.none)

    PianoMsg (Piano.KeyDown k) ->
      let oNote = (fromMidiNote k) in
      ( model
      , Cmd.batch 
        [ playNote oNote
        , Task.perform (flip SetPress oNote) Time.now
        ]
      )

    PianoMsg (Piano.KeyUp k) ->
      let oNote = (fromMidiNote k) in
      ( model
      , Cmd.batch
        [ stopNote (fromMidiNote k)
        , Task.perform (flip StoreNote oNote) Time.now
        ]
      )

    PianoMsg pmsg -> Debug.log (toString pmsg) (model, Cmd.none)

    SetPress time oNote ->
      ( { model | pressedNotes = addPressedNote model.pressedNotes time oNote } --noteHistory = (time, oNote) :: model.noteHistory } 
      , Cmd.none
      )

    StoreNote endTime oNote -> Debug.log (toString model.pressedNotes)
      ( { model | pressedNotes = removePressedNote model.pressedNotes oNote,
          noteHistory = 
            case Dict.get (toString oNote) model.pressedNotes of
              Just (startTime, oNote) -> {startTime=startTime, endTime=endTime, oNote=oNote} :: model.noteHistory
              Nothing -> model.noteHistory
            
        } 
      , Cmd.none
      )
-- VIEW

view : Model -> Html.Html Msg
view model =
  let
    pianoModel = 
      { notes = Set.fromList <| List.map (toMidiNote << Tuple.second) (Dict.values model.pressedNotes)
      , noteRange = (0, 35)
      , interactive = True
      , showSizeSelector = False
      , debugNotes = False
      }
  in
    div []
      [ Html.map PianoMsg 
        ( Piano.view pianoModel )
      , div [] [ text (toString model)]
      ]

toMidiNote : ONote -> Int
toMidiNote {note, octave} =
  octave * 12
    + case note of
      C -> 0
      CS -> 1
      D -> 2
      DS -> 3
      E -> 4
      F -> 5
      FS -> 6
      G -> 7
      GS -> 8
      A -> 9
      AS -> 10
      B -> 11

fromMidiNote : Int -> ONote
fromMidiNote midiNote =
  let 
    octave = floor (toFloat midiNote / 12)
    relativeNote = rem midiNote 12
  in
    case relativeNote of
      0 -> { note = C, octave = octave }
      1 -> { note = CS, octave = octave }
      2 -> { note = D, octave = octave }
      3 -> { note = DS, octave = octave }
      4 -> { note = E, octave = octave }
      5 -> { note = F, octave = octave }
      6 -> { note = FS, octave = octave }
      7 -> { note = G, octave = octave }
      8 -> { note = GS, octave = octave }
      9 -> { note = A, octave = octave }
      10 -> { note = AS, octave = octave }
      11 -> { note = B, octave = octave }
      _ -> { note = C, octave = octave }
  


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
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