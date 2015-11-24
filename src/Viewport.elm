module Viewport where

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

fromDimensions : (Int,Int) -> Int -> Int -> Viewport
fromDimensions (width,height) rows gutter =
  let
    halfWidth =
      width // 2

    halfHeight =
      height // 2

    minX =
      negate halfWidth |> toFloat

    maxX =
      halfWidth |> toFloat

    minY =
      negate halfHeight |> toFloat

    maxY =
      halfHeight |> toFloat

    minDimension =
      List.minimum [width, height] |> Maybe.withDefault 20

    squareWidth =
      (minDimension // rows) - ((rows - 1) * gutter) |> toFloat

    halfSquareWidth =
      squareWidth / 2

    boardWidth =
      let
        squareWidths = (squareWidth * (toFloat rows)) |> round
        gutterWidths = (gutter * (rows - 1))
      in
        toFloat (squareWidths + gutterWidths)

    halfBoardWidth =
      boardWidth / 2
  in
    Viewport
      width height halfWidth halfHeight
      minDimension minX maxX minY maxY
      squareWidth halfSquareWidth
      boardWidth halfBoardWidth
