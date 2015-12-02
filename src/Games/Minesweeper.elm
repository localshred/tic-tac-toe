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

type Content =
  PendingMineLayout
  | Touching Int
  | Mine

type Visibility =
  Hidden
  | Flagged
  | Peeking
  | Visible

type alias Square =
  (SquarePos,Content,Visibility)

type GameState =
  Pending
  | Started
  | Win
  | Loss

type Mode =
  Beginner
  | Intermediate
  | Advanced

type Action =
  Restart
  | ModeSelect Mode
  | SelectSquare SquarePos
  | PeekSquare SquarePos

type alias Model =
  { state : GameState
  , mode : Mode
  , dimensions : (Width,Height)
  , mineCount : Int
  , board : List (List Square)
  }

init : Model
init =
  Model Pending Beginner (0,0) 0 []

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
      , (String.toLower <| toString model.mode, True)
      ]
  in
    div [ boardClasses ] [ boardView ]

pendingView : Signal.Address Action -> Model -> Html
pendingView address model =
  div [ style [ ("text-align","center") ] ] [
    text "Start Game: "
    , UI.pureButton (onClick address (ModeSelect Beginner)) "Beginner"
    , UI.pureButton (onClick address (ModeSelect Intermediate)) "Intermediate"
    , UI.pureButton (onClick address (ModeSelect Advanced)) "Advanced"
  ]

playingView : Signal.Address Action -> Model -> Html
playingView address model =
  let
    printedRows =
      div [] <| List.map printRow model.board

    controlRow =
      div [ class "row" ] [
        text <| "Clock"
        , text <| "Playing! " ++ toString model.mode
        , UI.pureButton (onClick address Restart) ":)"
        , text <| "Score"
      ]

  in
    div [] <| controlRow :: printedRows :: []

printSquare : Square -> Html
printSquare square =
  let
    (pos, content, visibility) =
      square

    marker =
      case content of
        PendingMineLayout ->
          "P"

        Touching count ->
          toString count

        Mine ->
          "X"
  in
    div [ class "square" ] [
      text marker
    ]

printRow : List Square -> Html
printRow squares =
  List.map printSquare squares
  |> div [ class "row" ]

gameOverView : Signal.Address Action -> Model -> Html
gameOverView address model =
  div [] [ text "Game over!" ]

update : Action -> Model -> Model
update action model =
  case action of
    ModeSelect mode ->
      let
        model' = selectMode mode model
        board = generateBoard model'.dimensions model'.mineCount
      in
        { model' | state = Started
        , board = board }

    Restart ->
      init

    otherwise ->
      model

selectMode : Mode -> Model -> Model
selectMode mode model =
  case mode of
    Beginner ->
      { model | mode = mode
      , dimensions = (9,9)
      , mineCount = 10 }

    Intermediate ->
      { model | mode = mode
      , dimensions = (16,16)
      , mineCount = 40 }

    Advanced ->
      { model | mode = mode
      , dimensions = (30,16)
      , mineCount = 99 }

generateBoard : (Width,Height) -> Int -> List (List Square)
generateBoard (w,h) mineCount =
  let
    totalSquareCount =
      w * h

    mineLocationGenerator =
      Random.list mineCount (Random.int 0 <| totalSquareCount + 1)

    mineCellNumbers =
      Debug.log "randomInts" <|
        (Random.generate mineLocationGenerator (Random.initialSeed 42)
        |> fst
        |> List.sort
        |> List.map (\v -> v - 1))


    mineSquarePositions =
      List.map (cellNumberToSquarePosition w) mineCellNumbers

    mineNeighbors =
      List.foldl (List.append) [] (List.map (neighbors w h) mineSquarePositions)

    -- makeRow : Int -> Int -> List Square -> List Square
    makeRow row squaresPerRow rowSquares =
      if List.length rowSquares == squaresPerRow then
        rowSquares

      else
        let
          nextIndex =
            List.length rowSquares

          translatedCellNumber =
            (row * squaresPerRow) + nextIndex

          squarePos =
            (row, nextIndex)

          itemType =
            --if List.member translatedCellNumber mineLocations then
            if List.member squarePos mineSquarePositions then
              Mine

            else
              let
                touchingCount =
                  List.filter (\pos -> pos == squarePos) mineNeighbors
                  |> List.length
              in
                Touching touchingCount

          square =
            (squarePos, itemType, Hidden)
        in
          List.append rowSquares [ square ]
          |> makeRow row squaresPerRow

    -- makeRows : Int -> Int -> List (List Square)
    makeRows rowCount squaresPerRow rows =
      if List.length rows == rowCount then
        rows

      else
        let
          nextIndex =
            List.length rows

          row =
            makeRow nextIndex squaresPerRow []
        in
          List.append rows [ row ]
          |> makeRows rowCount squaresPerRow
  in
    Debug.log "board" <| makeRows (Debug.log "height" h) (Debug.log "width" w) []


cellNumberToSquarePosition : Width -> Int -> SquarePos
cellNumberToSquarePosition width cell =
  let
    row =
      cell // width

    col =
      cell % width
  in
    (,) row col

neighbors : Width -> Height -> SquarePos -> List SquarePos
neighbors w h (row,col) =
  let
    top = row - 1
    middle = row
    bottom = row + 1
    left = col - 1
    center = col
    right = col + 1

    candidates =
      [ (top,left),(top,center),(top,right)
      , (middle,left),(middle,center),(middle,right)
      , (bottom,left),(bottom,center),(bottom,right)
      ]

    neighborFilter (r,c) =
      r > -1 && c > -1 && r < h && c < w
  in
    List.filter neighborFilter candidates
    |> Debug.log ("neighbors " ++ toString (row,col))


