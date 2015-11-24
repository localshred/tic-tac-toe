module Game where

import Array exposing (initialize, repeat, toList)
import Color exposing (yellow, gray, blue, green, orange, red)
import Graphics.Collage exposing (Form, toForm, collage, moveY, move, outlined, filled, dashed, square, circle, solid, scale)
import Graphics.Element exposing (Element, rightAligned, leftAligned, show)
import Mouse
import Text exposing (fromString)
import Window
import Debug

import Viewport exposing (Viewport)

cols : number
cols =
  3

gutter : number
gutter =
  10

rows : number
rows =
  3

type Player =
  X | O

type State =
  Pending
  | Started
  | Winner Player
  | Stalemate

type alias SquarePosition =
  (Int,Int,Player)

type alias Game =
  { selections : List SquarePosition
  , state : State
  , nextTurn : Player
  }

init : Game
init =
  Game [] Pending X

advanceTurn : Player -> Player
advanceTurn currentPlayer =
  case currentPlayer of
    X ->
      O

    O ->
      X

main : Signal Element
main =
  let
    mouseDownSampling =
      Signal.sampleOn Mouse.isDown Mouse.position

    game =
      Signal.foldp update init mouseDownSampling
  in
    Signal.map3 view Mouse.position Window.dimensions game

view : (Int,Int) -> (Int,Int) -> Game -> Element
view mousePosition dimensions game =
  let
    viewport =
      Viewport.fromDimensions dimensions rows gutter

    gameRows =
      makeRows rows cols viewport

    centerCircle =
      filled yellow (circle 10)

    title =
      drawTitle viewport

    gameStateDisplay =
      gameState viewport game

    mouseForm =
      mouseView viewport mousePosition

    canvas forms =
      collage viewport.width viewport.height forms

    forms =
      title :: centerCircle :: mouseForm :: gameStateDisplay :: gameRows
  in
    canvas forms

update : (Int,Int) -> Game -> Game
update (x,y) previousGame  =
  { previousGame |
    nextTurn = advanceTurn previousGame.nextTurn
    , state = Started
  }

mouseView : Viewport -> (Int,Int) -> Form
mouseView viewport mousePosition =
  show mousePosition
  |> toForm
  |> move ((,) (viewport.minX + 50) (negate (viewport.minY + 30)))

gameState : Viewport -> Game -> Form
gameState viewport game =
  let
    xy = (,) (negate (viewport.minX + 50)) (viewport.minY + 90)
    state = case game.state of
              Pending -> "Pending"
              Started -> "Started"
              Winner player -> "Player " ++ playerName player ++ " won!"
              Stalemate -> "Aww shucks, stalemate!"
  in
    state
    |> fromString
    |> rightAligned
    |> toForm
    |> move xy

playerName : Player -> String
playerName player =
  case player of
    X -> "X"
    O -> "O"

drawTitle : Viewport -> Form
drawTitle viewport =
  let
    xy = (,) (negate (viewport.minX + 50)) (negate (viewport.minY + 90))
    possibleScales = [ 1.0, (viewport.minDimension // 225 |> toFloat) ]
    factor = List.maximum possibleScales |> Maybe.withDefault 1.0
  in
    "Tic\nTac\nToe"
    |> fromString
    |> leftAligned
    |> toForm
    |> move xy
    |> scale factor

makeRows : Int -> Int -> Viewport -> List Form
makeRows rows cols viewport =
  [ makeSquare 0 0 viewport green
  , makeSquare 0 1 viewport green
  , makeSquare 0 2 viewport green
  , makeSquare 1 0 viewport blue
  , makeSquare 1 1 viewport blue
  , makeSquare 1 2 viewport blue
  , makeSquare 2 0 viewport red
  , makeSquare 2 1 viewport red
  , makeSquare 2 2 viewport red
  ]

makeSquare : Int -> Int -> Viewport -> Color.Color -> Form
makeSquare row col viewport color =
  let
    coordinates = squarePosition row col viewport
  in
    outlined (dashed color) (square viewport.squareWidth)
    |> move coordinates


offset : Int -> Viewport -> Float
offset pos viewport =
  let
    leftRight =
      case pos of
        0 ->
          1

        1 ->
          0

        2 ->
          -1

        otherwise ->
          0

    operands =
      [ (viewport.halfBoardWidth * -1)
      , ((toFloat pos) * viewport.halfBoardWidth)
      , (viewport.halfSquareWidth * leftRight)
      ]
  in
    List.foldr (+) 0 operands

squarePosition : Int -> Int -> Viewport -> (Float,Float)
squarePosition row col viewport =
  let
    x =
      offset col viewport

    y =
      offset row viewport
  in
    (,) x y

