module Games.Minesweeper (Action, Model, view, update, init, inputs) where

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
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
  | Incorrect

type Visibility =
  Covered
  | Uncovered
  | Flagged FlagType
  | Peek

type alias Square =
  { pos : SquarePos
  , content : Content
  , visibility : Visibility
  }

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
  | MetaKeyDown Bool
  | ModeSelect Mode
  | SelectSquare Square
  | PeekSquare Square

type alias Dimensions =
  (Width, Height)

type alias TouchingNeighbors =
  Dict SquarePos (List SquarePos)

type alias Model =
  { state : GameState
  , mode : Mode
  , dimensions : Dimensions
  , mineCount : Int
  , board : List (List Square)
  , metaKeyDown : Bool
  }

inputs : List (Signal Action)
inputs =
  [ Signal.map MetaKeyDown Keyboard.meta
  ]

init : Model
init =
  Model Pending Beginner (0,0) 0 [] False
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
        score model
        , text <| stateText
        , UI.pureButton (onClick address Restart) "🙂"
        , div [ class "clock" ] [ text "000" ]
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
          |> List.filter (\square -> square.visibility == Flagged Flag)
          |> List.length
          |> (-) model.mineCount

        Win ->
          0

        otherwise ->
          model.mineCount
  in
    div [ class "score" ] [
      text <| toString flaggedCount
    ]

printSquare : Signal.Address Action -> Model -> Square -> Html
printSquare address model square =
  let
    classes =
      classList [ ("square", True)
      , ("flagged flagged-flag", square.visibility == Flagged Flag)
      , ("flagged flagged-question", square.visibility == Flagged Question)
      , ("flagged flagged-incorrect", square.visibility == Flagged Incorrect)
      , ("covered", square.visibility == Covered)
      , ("peek", square.visibility == Peek)
      , ("uncovered", square.visibility == Uncovered)
      , ("mine", square.content == Mine)
      , ("exploded-mine", square.content == ExplodedMine)
      , ("touching touching0", square.content == (Touching 0))
      , ("touching touching1", square.content == (Touching 1))
      , ("touching touching2", square.content == (Touching 2))
      , ("touching touching3", square.content == (Touching 3))
      , ("touching touching4", square.content == (Touching 4))
      , ("touching touching5", square.content == (Touching 5))
      , ("touching touching6", square.content == (Touching 6))
      , ("touching touching7", square.content == (Touching 7))
      , ("touching touching8", square.content == (Touching 8))
      ]

    onClickHandler =
      onClick address (SelectSquare square)

    squareAttributes =
      if model.state == Started && square.content == (Touching 0) && square.visibility == Uncovered then
        [ classes ]
      else if model.state == Started then
        [ classes, onClickHandler ]
      else
        [ classes ]
  in
    div squareAttributes [
      text <| marker square
    ]

marker : Square -> String
marker square =
  case square.visibility of
    Flagged Flag ->
      "🚩"

    Flagged Question ->
      "❓"

    Flagged Incorrect ->
      "❌"

    Uncovered ->
      case square.content of
        Touching count ->
          if count == 0 then
            ""

          else
            toString count

        Mine ->
          "💣"

        ExplodedMine ->
          "💥"

    Covered ->
      ""

    Peek ->
      ""

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
      if model.metaKeyDown then
        flagSquareInBoard model square

      else
        updateSquareSelection model square
        |> promoteToWin


    MetaKeyDown keyState ->
      { model | metaKeyDown = keyState }

    otherwise ->
      model

promoteToWin : Model -> Model
promoteToWin model =
  let
    uncoveredCount =
      List.concat model.board
      |> List.filter (\square -> square.visibility == Uncovered)
      |> List.length

    squareCount =
      (fst model.dimensions) * (snd model.dimensions)

    uncoveredSquareCountTriggeringWin =
      squareCount - model.mineCount

    model' =
      if uncoveredCount == uncoveredSquareCountTriggeringWin then
        { model | state = Win
        , board = flagAllMines model.board
        }

      else
        model
  in
    model'

flagAllMines : List (List Square) -> List (List Square)
flagAllMines rows =
  let
    flagMineSquare square =
      if square.content == Mine then
        { square | visibility = Flagged Flag }
      else
        square

    updateRow squares =
      List.map flagMineSquare squares

    rows' =
      List.map updateRow rows
  in
    rows'

flaggedNeighboringMineCount : Model -> Square -> Int
flaggedNeighboringMineCount model square =
  model.board
  |> List.concat
  |> List.filter (\square -> square.visibility == Flagged Flag)
  |> List.length

{-
TODO:
  [√] covered -> touching > 0 -> uncover this square
  [√] covered -> mine -> explode all mines
  [√] covered -> touching 0 -> uncover this square and all neighbors that are not mines, recursively
  [√] uncovered -> touching 0 -> do nothing
  [-] uncovered -> touch > 0 && neighbor flag count == touching count -> uncover all neighbors
  [-] uncovered -> touching > 0 && neighbor flag count != touching count -> peek while mouse is down
-}
updateSquareSelection : Model -> Square -> Model
updateSquareSelection model square =
  case square.content of
    Touching 0 ->
      uncoverSquareAndNeighbors model square

    Touching count ->
      let
        flaggedCount =
          flaggedNeighboringMineCount model square
      in
        if count == flaggedCount then
          -- TODO verify this after we get keyboard flagging in
          uncoverSquareAndNeighbors model square

        else
          -- TODO peek while mouse is down
          uncoverSquareInBoard model square

    Mine ->
      mineExploded model square

    otherwise ->
      model

eqSquare : SquarePos -> SquarePos -> Bool
eqSquare a b =
  a == b

mineExploded : Model -> Square -> Model
mineExploded model square =
  let
    squareExploded square' =
      if eqSquare square.pos square'.pos then
        { square | content = ExplodedMine, visibility = Uncovered }
      else
        { square' | visibility = Uncovered }

    updateRow row =
      List.map squareExploded row

    updatedBoard =
      List.map updateRow model.board
  in
    { model | board = updatedBoard
    , state = Loss
    }

linkedTouchingNeighbors : Model -> SquarePos -> TouchingNeighbors -> TouchingNeighbors
linkedTouchingNeighbors model pos neighborsDict =
  if Dict.member pos neighborsDict then
    neighborsDict

  else
    let
      squareIsTouching0 =
        model.board
        |> List.concat
        |> List.filter (\square -> eqSquare square.pos pos)
        |> List.head
        |> Maybe.map (\square -> square.content == Touching 0)
        |> Maybe.withDefault False
    in
      if squareIsTouching0 then
        let
          neighbors' =
            neighbors model.dimensions pos
            |> (::) pos

          neighborIsTouching square =
            case square.content of
              Touching n ->
                List.member square.pos neighbors'

              otherwise ->
                False

          neighborPositions =
            model.board
            |> List.concat
            |> List.filter neighborIsTouching
            |> List.map .pos

          currentDict =
            Dict.insert pos neighborPositions neighborsDict

          neighborsDict' =
            List.foldl (linkedTouchingNeighbors model) currentDict neighbors'
        in
          neighborsDict'

      else
        neighborsDict

flagSquare : Square -> Square -> Square
flagSquare selectedSquare square =
  if selectedSquare.pos /= square.pos then
    square

  else
    case selectedSquare.visibility of
      Covered ->
        { square | visibility = Flagged Flag }

      Flagged Flag ->
        { square | visibility = Flagged Question }

      Flagged Question ->
        { square | visibility = Covered }

      otherwise ->
        square

uncoverSquare : Square -> Square -> Square
uncoverSquare selectedSquare square =
  if selectedSquare.pos == square.pos then
    { square | visibility = Uncovered }
  else
    square

uncoverMatchingSquares : List Square -> Square -> Square
uncoverMatchingSquares squares square =
  if List.member square squares then
    { square | visibility = Uncovered }
  else
    square

flagSquareInBoard : Model -> Square -> Model
flagSquareInBoard model square =
  let
    updateRow row =
      List.map (flagSquare square) row

    updatedBoard =
      List.map updateRow model.board
  in
    { model | board = updatedBoard }

uncoverSquareInBoard : Model -> Square -> Model
uncoverSquareInBoard model square =
  let
    updateRow row =
      List.map (uncoverSquare square) row

    updatedBoard =
      List.map updateRow model.board
  in
    { model | board = updatedBoard }

uncoverSquareAndNeighbors : Model -> Square -> Model
uncoverSquareAndNeighbors model square =
  let
    linkedNeighbors =
      linkedTouchingNeighbors model square.pos Dict.empty
      |> Dict.values
      |> Set.fromList
      |> Set.toList
      |> List.concat

    findNeighborSquares square' =
      List.member square'.pos linkedNeighbors

    neighborSquares =
      model.board
      |> List.concat
      |> List.filter findNeighborSquares

    updateRow row =
      List.map (uncoverMatchingSquares neighborSquares) row

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
      Random.generate (mineLocationGenerator count maxSize) (Random.initialSeed seed)
      |> fst
      |> List.sort
      |> List.map dec
      |> Set.fromList
      |> Set.union initialMines

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
      List.map (neighbors (width,height)) mineSquarePositions
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

          content =
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
            Square squarePos content Covered
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
     makeRows height width []

cellNumberToSquarePosition : Width -> Int -> SquarePos
cellNumberToSquarePosition width cell =
  let
    row =
      cell // width

    col =
      cell % width
  in
    (,) row col

neighbors : Dimensions -> SquarePos -> List SquarePos
neighbors (width, height) (row, col) =
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
