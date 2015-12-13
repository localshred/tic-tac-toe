module Games.Minesweeper (Action, Model, view, update, init, inputs) where

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard
import Random
import Set exposing (Set)
import String
import Time exposing (Time)
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
  | Incorrect

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
  | Tick Time
  | ModeSelect Mode
  | SelectSquare Square
  | PeekSquare Square
  | UncoverNeighborSquares Square

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
  , startTime : Time
  , currentTime : Time
  , elapsedTime : Int
  }

{-
  TODO
  - [ ] Get time at some value to get a better random seed on new game
-}
inputs : Signal Action
inputs =
  Signal.mergeMany [ Signal.map MetaKeyDown Keyboard.meta
  , Signal.map Tick (Time.every Time.second)
  ]

init : Model
init =
  Model Pending Beginner (0,0) 0 [] False 0 0 0
  |> update (ModeSelect Beginner) -- FIXME remove this to get back to allowing mode selection

newGame : Model -> Model
newGame model =
  { model | state = Started
  , board = generateBoard model.currentTime model.dimensions model.mineCount
  , startTime = 0
  , elapsedTime = 0
  }

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

    controlRow =
      div [ class "row controls" ] [
        score model
        , restartButton address model
        , div [ class "clock" ] [ text <| padNumber model.elapsedTime ]
      ]

    gameInfo =
      ul [ class "game-info" ] [
        li [] [ text "Left click to uncover a square." ]
        , li [] [ text "Click and hold on an uncovered square to peek at which neighbors it touches." ]
        , li [] [ text "⌘ click to plant a flag over a suspected mine. ⌘ click again to make it a question mark." ]
        , li [] [ text "The game is won when all squares not containing mines have been uncovered." ]
      ]
  in
    div [] <| controlRow :: boardRows :: gameInfo :: []

padNumber : number -> String
padNumber n =
  if n < 10 then
    "00" ++ toString n

  else if n < 100 then
    "0" ++ toString n

  else
    toString n


    

restartButton : Signal.Address Action -> Model -> Html
restartButton address model =
  let
    restartEmoji =
      case model.state of
        Started ->
          "🙂"

        Win ->
          "😎"

        Loss ->
          "😭"

        Pending ->
          "💤"
  in
    div [ class "row" ] [
      UI.pureButton (onClick address Restart) restartEmoji
    ]

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

    stringFlagCount =
      if flaggedCount < 10 then
        "00" ++ toString flaggedCount

      else if flaggedCount < 100 then
        "0" ++ toString flaggedCount

      else
        toString flaggedCount
  in
    div [ class "score" ] [
      text <| stringFlagCount
    ]

printSquare : Signal.Address Action -> Model -> Square -> Html
printSquare address model square =
  let
    classes =
      classList [ ("square", True)
      , ("flagged flagged-flag", square.visibility == Flagged Flag)
      , ("flagged flagged-question", square.visibility == Flagged Question)
      , ("incorrect", square.visibility == Incorrect)
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

    uncoverSquareHandler =
      onClick address (SelectSquare square)

    peekDownHandler =
      onMouseDown address (PeekSquare square)

    peekUpHandler =
      onMouseUp address (PeekSquare square)

    uncoverNeighborsHandler =
      onClick address (UncoverNeighborSquares square)

    squareAttributes =
      if model.state == Started && square.content == (Touching 0) && square.visibility == Uncovered then
        [ classes ]

      else if squareNeighborFlagsEqualTouchingCount model square then
        [ classes, uncoverNeighborsHandler ]

      else if shouldAddPeekHandlers model square then
        [ classes, peekDownHandler, peekUpHandler ]

      else if model.state == Started then
        [ classes, uncoverSquareHandler ]

      else
        [ classes ]
  in
    div squareAttributes [
      text <| marker square
    ]

squareNeighborFlagsEqualTouchingCount : Model -> Square -> Bool
squareNeighborFlagsEqualTouchingCount model square =
  if square.visibility == Uncovered && isTouchingMoreThanZero square then
    case square.content of
      Touching touchingCount ->
        let
          linkedNeighborPositions =
            neighbors model.dimensions square.pos

          findNeighborSquares square' =
            List.member square'.pos linkedNeighborPositions
              && square'.visibility == Flagged Flag

          flaggedCount =
            model.board
            |> List.concat
            |> List.filter findNeighborSquares
            |> List.length
        in
          touchingCount == flaggedCount

      otherwise ->
        False

  else
    False

shouldAddPeekHandlers : Model -> Square -> Bool
shouldAddPeekHandlers model square =
  model.state == Started
    && (isTouchingMoreThanZero square)
    && square.visibility == Uncovered

isTouchingMoreThanZero : Square -> Bool
isTouchingMoreThanZero square =
  case square.content of
    Touching 0 ->
      False

    Touching n ->
      True

    otherwise ->
      False

marker : Square -> String
marker square =
  case square.visibility of
    Flagged Flag ->
      "🚩"

    Flagged Question ->
      "❓"

    Incorrect ->
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

initializeStartTime : Model -> Model
initializeStartTime model =
  case model.state of
    Started ->
      if model.startTime == 0 && model.currentTime > 0 then
        { model | startTime = model.currentTime }

      else
        model

    otherwise ->
      model

update : Action -> Model -> Model
update action model =
  case action of
    ModeSelect mode ->
      let
        model' = selectMode mode model
        board = generateBoard model.currentTime model'.dimensions model'.mineCount
      in
        { model' | state = Started
        , board = board }

    Restart ->
      newGame model

    SelectSquare square ->
      if model.metaKeyDown then
        flagSquareInBoard model square
        |> initializeStartTime

      else
        updateSquareSelection model square
        |> promoteToWinOrLoss
        |> initializeStartTime

    PeekSquare square ->
      peekSquareNeighbors model square

    UncoverNeighborSquares square ->
      uncoverSquareNeighbors model square
      |> promoteToWinOrLoss
      |> initializeStartTime

    MetaKeyDown keyState ->
      { model | metaKeyDown = keyState }

    Tick time ->
      { model | currentTime = time }
      |> updateElapsedTime

updateElapsedTime : Model -> Model
updateElapsedTime model =
  case model.state of
    Started ->
      if model.startTime > 0 && model.currentTime > 0 then
        { model |
          elapsedTime =
            floor <| (model.currentTime - model.startTime) / 1000
        }

      else
        model

    otherwise ->
      model

promoteToWinOrLoss : Model -> Model
promoteToWinOrLoss model =
  let
    lossModel = promoteToLoss model
    winModel = promoteToWin model
  in
    if lossModel.state == Loss then
      lossModel

    else if winModel.state == Win then
      winModel

    else
      model

promoteToLoss : Model -> Model
promoteToLoss model =
  let
    uncoveredMinesCount =
      List.concat model.board
      |> List.filter (\square -> square.visibility == Uncovered && square.content == Mine)
      |> List.length

    model' =
      if uncoveredMinesCount > 0 then
        { model | state = Loss
        , elapsedTime = 999
        , board = uncoverAllSquaresAndShowIncorrectFlags model.board
        }

      else
        model
  in
    model'

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

peekSquareNeighbors : Model -> Square -> Model
peekSquareNeighbors model square =
  let
    linkedNeighborPositions =
      neighbors model.dimensions square.pos

    findNeighborSquares square' =
      List.member square'.pos linkedNeighborPositions

    neighborSquares =
      model.board
      |> List.concat
      |> List.filter findNeighborSquares

    updateRow row =
      List.map (peekMatchingSquares neighborSquares) row

    updatedBoard =
      List.map updateRow model.board
  in
    { model | board = updatedBoard }

uncoverSquareNeighbors : Model -> Square -> Model
uncoverSquareNeighbors model square =
  let
    linkedNeighborPositions =
      neighbors model.dimensions square.pos

    findNeighborSquares square' =
      List.member square'.pos linkedNeighborPositions
        && square'.visibility == Covered

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

uncoverAllSquaresAndShowIncorrectFlags : List (List Square) -> List (List Square)
uncoverAllSquaresAndShowIncorrectFlags rows =
  let
    flagMineSquare square =
      if square.content /= Mine && square.visibility == Flagged Flag then
        { square | visibility = Incorrect }

      else if square.content /= Mine && square.visibility == Flagged Question then
        { square | visibility = Incorrect }

      else if square.visibility == Covered || square.visibility == Peek then
        { square | visibility = Uncovered }

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
          uncoverSquareAndNeighbors model square

        else
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

peekMatchingSquares : List Square -> Square -> Square
peekMatchingSquares squares square =
  if List.member square squares && square.visibility == Covered then
    { square | visibility = Peek }

  else if List.member square squares && square.visibility == Peek then
    { square | visibility = Covered }

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

generateBoard : Time -> (Width,Height) -> Int -> List (List Square)
generateBoard time (width,height) mineCount =
  let
    totalSquareCount =
      width * height

    mineSquarePositions =
      Set.map (cellNumberToSquarePosition width) (generateRandomMineCells mineCount totalSquareCount (floor time) Set.empty)
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
