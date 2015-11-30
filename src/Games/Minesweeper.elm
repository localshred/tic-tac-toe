module Games.Minesweeper (Action, Model, view, update, init) where

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import String
import UI

type alias Row =
  Int

type alias Col =
  Int

type alias Width =
  Int

type alias Height =
  Int

type alias SquarePos =
  (Row,Col)

type Item =
  PendingBombLayout
  | Touching Int
  | Bomb

type Visibility =
  Hidden
  | Peeking
  | Visible

type alias Square =
  (SquarePos,Item,Visibility)

type GameState =
  Pending
  | Started
  | Win
  | Loss

type Action =
  Restart
  | SetGameSize (Width,Height) Int
  | SelectSquare SquarePos
  | PeekSquare SquarePos

type alias Model =
  { dimensions : (Width,Height)
  , squares : List (List Square)
  , state : GameState
  }

init : Model
init =
  Model (0,0) [] Pending

view : Signal.Address Action -> Model -> Html
view address model =
  let
    boardView =
      case model.state of
        Pending ->
          pendingView address model

        Started ->
          playingView address model

        otherwise ->
          gameOverView address model

    boardClasses =
      classList [ ("board", True)
      , (String.toLower <| toString model.state, True)
      ]
  in
    div [ boardClasses ] [ boardView ]

pendingView : Signal.Address Action -> Model -> Html
pendingView address model =
  div [ style [ ("text-align","center") ] ] [
    text "Start Game: "
    , UI.pureButton (onClick address (SetGameSize (9,9) 10)) "Beginner"
    , UI.pureButton (onClick address (SetGameSize (16,16) 40)) "Intermediate"
    , UI.pureButton (onClick address (SetGameSize (30,16) 99)) "Advanced"
  ]

playingView : Signal.Address Action -> Model -> Html
playingView address model =
  div [] [ text "Playing!" ]

gameOverView : Signal.Address Action -> Model -> Html
gameOverView address model =
  div [] [ text "Game over!" ]

update : Action -> Model -> Model
update action model =
  case action of
    SetGameSize dimensions mineCount ->
      { model | state = Started
      , dimensions = dimensions
      , squares = generateBoard dimensions mineCount }

    otherwise ->
      model

generateBoard : (Width,Height) -> Int -> List (List Square)
generateBoard (w,h) mineCount =
  let
    squareCount =
      w * h

    bombLocationGenerator =
      Random.list mineCount (Random.int 0 <| squareCount + 1)

    bombLocations =
      Debug.log "randomInts" <|
        (Random.generate bombLocationGenerator (Random.initialSeed 42) |> fst |> List.sort |> List.map (\v -> v - 1))

    -- makeRow : Int -> Int -> List Square -> List Square
    makeRow row sqaureCount rowSquares =
      if List.length rowSquares == squareCount then
        rowSquares

      else
        let
          nextIndex =
            List.length rowSquares

          translatedCellNumber =
            row + nextIndex

          itemType =
            if List.member translatedCellNumber bombLocations then
              Bomb

            else
              PendingBombLayout

          square =
            ((row, nextIndex), itemType, Hidden)
        in
          List.append rowSquares [ square ]
          |> makeRow row squareCount

    -- makeRows : Int -> Int -> List (List Square)
    makeRows rowCount squareCount rows =
      if List.length rows == rowCount then
        rows

      else
        let
          nextIndex =
            List.length rows

          row =
            makeRow nextIndex squareCount []
        in
          List.append rows [ row ]

  in
    Debug.log "board" <| makeRows h w []

