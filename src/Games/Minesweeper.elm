module Games.Minesweeper (Action, Model, view, update, init) where

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Set exposing (Set)
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
  Touching Int
  | Mine
  | ExplodedMine

type FlagType =
  Flag
  | Question

type Visibility =
  Covered
  | Uncovered
  | Flagged FlagType
  | Peek

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
  | SelectSquare Square
  | FlagSquare Square
  | PeekSquare Square

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
  |> update (ModeSelect Beginner) -- FIXME remove this to get back to allowing mode selection

view : Signal.Address Action -> Model -> Html
view address model =
  let
    board =
      case model.state of
        Pending ->
          pendingView address model

        otherwise ->
          boardView address model

    boardClasses =
      classList [ ("board", True)
      , (String.toLower <| toString model.state, True)
      , (String.toLower <| toString model.mode, True)
      ]
  in
    div [ boardClasses ] [ board ]

pendingView : Signal.Address Action -> Model -> Html
pendingView address model =
  div [ style [ ("text-align","center") ] ] [
    text "Start Game: "
    , UI.pureButton (onClick address (ModeSelect Beginner)) "Beginner"
    , UI.pureButton (onClick address (ModeSelect Intermediate)) "Intermediate"
    , UI.pureButton (onClick address (ModeSelect Advanced)) "Advanced"
  ]

boardView : Signal.Address Action -> Model -> Html
boardView address model =
  let
    boardRows =
      div [] <| List.map (printRow address model) model.board

    stateText =
      case model.state of
        Started ->
          "Playing " ++ toString model.mode ++ " mode!"

        Win ->
          "You totally won!"

        Loss ->
          "You totally lost!"

        Pending ->
          "Start the game already!"

    controlRow =
      div [ class "row" ] [
        text <| "Clock"
        , text <| stateText
        , UI.pureButton (onClick address Restart) ":)"
        , score model
      ]

    gameInfo =
      ul [ class "game-info" ] [
        li [] [ text "Left click to uncover a square." ]
        , li [] [ text "Right click to plant a flag over a suspected mine. Right click again to make it a question mark." ]
        , li [] [ text "The game is won when all squares not containing mines have been uncovered." ]
      ]

  in
    div [] <| controlRow :: boardRows :: gameInfo :: []

score : Model -> Html
score model =
  let
    flaggedCount =
      case model.state of
        Started ->
          List.concat model.board
          |> List.filter (\(_,_,visibility) -> visibility == Flagged Flag)
          |> List.length

        otherwise ->
          999
  in
    div [ class "score" ] [
      text <| toString flaggedCount
    ]

printSquare : Signal.Address Action -> Model -> Square -> Html
printSquare address model square =
  let
    (pos, content, visibility) =
      square

    classes = classList [ ("square", True)
    , ("flagged flagged-flag", visibility == Flagged Flag)
    , ("flagged flagged-question", visibility == Flagged Question)
    , ("covered", visibility == Covered)
    , ("peek", visibility == Peek)
    , ("uncovered", visibility == Uncovered)
    , ("mine", content == Mine)
    , ("exploded-mine", content == ExplodedMine)
    , ("touching touching0", content == (Touching 0))
    , ("touching touching1", content == (Touching 1))
    , ("touching touching2", content == (Touching 2))
    , ("touching touching3", content == (Touching 3))
    , ("touching touching4", content == (Touching 4))
    , ("touching touching5", content == (Touching 5))
    , ("touching touching6", content == (Touching 6))
    , ("touching touching7", content == (Touching 7))
    , ("touching touching8", content == (Touching 8))
    ]

    marker =
      case content of
        Touching count ->
          toString count

        Mine ->
          "ø"

        ExplodedMine ->
          "✱"

    onClickHandler =
      onClick address (SelectSquare square)

    squareAttributes =
      if model.state == Started && content == (Touching 0) && visibility == Uncovered then
        [ classes ]
      else if model.state == Started then
        [ classes, onClickHandler ]
      else
        [ classes ]
  in
    div squareAttributes [
      text marker
    ]

printRow : Signal.Address Action -> Model -> List Square -> Html
printRow address model squares =
  List.map (printSquare address model) squares
  |> div [ class "row" ]

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

    SelectSquare square ->
      updateSquareSelection model square

    otherwise ->
      model

updateSelectedSquare : Square -> Square -> Square
updateSelectedSquare (selectedPos, selectedContent, selectedVisibility) (pos, content, visibility) =
  if selectedPos == pos then
    (selectedPos, selectedContent, Uncovered)
  else
    (pos, content, visibility)


{-
TODO:
  [√] covered -> touching > 0 -> uncover this square
  [√] covered -> mine -> explode all mines
  [ ] covered -> touching 0 -> uncover this square and all neighbors that are not mines, recursively
  [ ] uncovered -> touching 0 -> do nothing
  [ ] uncovered -> touching > 0 && neighbor flag count != touching count -> peek while mouse is down
  [ ] uncovered -> touch > 0 && neighbor flag count == touching count -> uncover all neighbors
-}
updateSquareSelection : Model -> Square -> Model
updateSquareSelection model square =
  let
    (_, content, _) =
      square
  in
    case content of
      Touching count ->
        uncoverSquare model square

      Mine ->
        mineExploded model square

      otherwise ->
        model

mineExploded : Model -> Square -> Model
mineExploded model square =
  let
    (pos, _, _) =
      square

    squareExploded (pos', content', _) =
      if pos == pos' then
        (pos, ExplodedMine, Uncovered)
      else
        (pos, content', Uncovered)

    updateRow row =
      List.map squareExploded row

    updatedBoard =
      List.map updateRow model.board
  in
    { model | board = updatedBoard
    , state = Loss
    }

uncoverSquare : Model -> Square -> Model
uncoverSquare model square =
  let
    updateRow row =
      List.map (updateSelectedSquare square) row

    updatedBoard =
      List.map updateRow model.board
  in
    { model | board = updatedBoard }

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

dec : Int -> Int
dec v =
  v - 1

mineLocationGenerator : Int -> Int -> Random.Generator (List Int)
mineLocationGenerator count maxSize =
  Random.list count (Random.int 0 <| maxSize + 1)

generateRandomMineCells : Int -> Int -> Int -> Set Int -> Set Int
generateRandomMineCells count maxSize seed initialMines =
  let
    randomMines =
      Debug.log "randomInts" <| (Random.generate (mineLocationGenerator count maxSize) (Random.initialSeed seed)
      |> fst
      |> List.sort
      |> List.map dec
      |> Set.fromList
      |> Set.union initialMines)

    missingMinesCount = count - (Set.size randomMines)
  in
    if missingMinesCount > 0 then
      generateRandomMineCells missingMinesCount maxSize (seed + 1) randomMines

    else
      randomMines

generateBoard : (Width,Height) -> Int -> List (List Square)
generateBoard (width,height) mineCount =
  let
    totalSquareCount =
      width * height

    mineSquarePositions =
      Set.map (cellNumberToSquarePosition width) (generateRandomMineCells mineCount totalSquareCount 42 Set.empty)
      |> Set.toList

    mineNeighbors =
      List.map (neighbors width height) mineSquarePositions
      |> List.foldl (List.append) []

    -- makeRow : Int -> Int -> List Square -> List Square
    makeRow row squaresPerRow rowSquares =
      if List.length rowSquares == squaresPerRow then
        rowSquares

      else
        let
          nextIndex =
            List.length rowSquares

          squarePos =
            (row, nextIndex)

          itemType =
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
            (squarePos, itemType, Covered)
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
     makeRows (Debug.log "height" height) (Debug.log "width" width) []
     |> Debug.log "board"

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
neighbors width height (row,col) =
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

    neighborFilter (row',col') =
      row' > -1 && col' > -1 && row' < height && col' < width
  in
    List.filter neighborFilter candidates
    |> Debug.log ("neighbors " ++ toString (row,col))


