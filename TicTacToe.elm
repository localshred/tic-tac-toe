module TicTacToe where

import Graphics.Element exposing (Element, leftAligned)
import Graphics.Collage exposing (Form, toForm, collage, moveY, move, outlined, filled, dashed, square, circle, solid, scale)
import Color exposing (yellow, gray, blue, green, orange, red)
import Text exposing (fromString)
import Window
import Signal exposing ((<~))
import Array exposing (initialize, repeat, toList)

gutter = 10
rows = 3
cols = 3

type alias Viewport =
  { width : Int
  , height : Int
  , halfWidth : Int
  , halfHeight : Int
  , minDimension : Int
  , minX : Float
  , maxX : Float
  , minY : Float
  , maxY : Float
  , squareWidth : Float
  , halfSquareWidth : Float
  , boardWidth : Float
  , halfBoardWidth : Float
  }

main : Signal Element
main =
  drawBoard <~ Window.dimensions

makeViewport : (Int,Int) -> Viewport
makeViewport (width,height) =
  let
    halfWidth = width // 2
    halfHeight = height // 2
    minX = negate halfWidth |> toFloat
    maxX = halfWidth |> toFloat
    minY = negate halfHeight |> toFloat
    maxY = halfHeight |> toFloat
    minDimension = List.minimum [width, height] |> Maybe.withDefault 20
    squareWidth = (minDimension // rows) - ((rows - 1) * gutter) |> toFloat
    halfSquareWidth = squareWidth / 2
    boardWidth = (squareWidth * rows) + (gutter * (rows - 1))
    halfBoardWidth = boardWidth / 2
  in
    (Viewport
      width height halfWidth halfHeight
      minDimension minX maxX minY maxY
      squareWidth halfSquareWidth
      boardWidth halfBoardWidth)

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


drawBoard : (Int,Int) -> Element
drawBoard dimensions =
  let
    viewport = makeViewport dimensions
    gameRows = makeRows rows cols viewport
    centerCircle = filled yellow (circle 10)
    title = drawTitle viewport
  in
    collage viewport.width viewport.height
    <| title :: centerCircle :: gameRows


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

offset : Int -> Viewport -> Float
offset pos viewport =
  let
    leftRight = case pos of
                 0 -> 1
                 1 -> 0
                 2 -> -1
                 otherwise -> 0
  in
    [ (viewport.halfBoardWidth * -1)
    , ((toFloat pos) * viewport.halfBoardWidth)
    , (viewport.halfSquareWidth * leftRight)
    ] |> List.foldr (+) 0

squarePosition : Int -> Int -> Viewport -> (Float,Float)
squarePosition row col viewport =
  let
    x = offset col viewport
    y = offset row viewport
  in
    (,) x y

makeSquare : Int -> Int -> Viewport -> Color.Color -> Form
makeSquare row col viewport color =
  let
    coordinates = squarePosition row col viewport
  in
    outlined (dashed color) (square viewport.squareWidth)
    |> move coordinates

