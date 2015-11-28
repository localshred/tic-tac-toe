module Main where

import Game
import Graphics.Element exposing (
  down
  , Element
  , flow
  , layers
  , leftAligned
  , rightAligned
  , show
  , spacer
  )
import Mouse
import Text exposing (fromString)
import Viewport exposing (Viewport)
import Window

gutter : number
gutter =
  10

rows : number
rows =
  3

main : Signal Element
main =
  Signal.map3 view Mouse.position Window.dimensions nextGameState

mouseDownSampling : Signal (Int, Int)
mouseDownSampling =
  Signal.sampleOn Mouse.clicks Mouse.position

nextGameState : Signal Game.Model
nextGameState =
  Signal.foldp Game.update Game.init mouseDownSampling

view : (Int,Int) -> (Int,Int) -> Game.Model -> Element
view mousePosition dimensions model =
  let
    viewport =
      Viewport.fromDimensions dimensions rows gutter

    gameBoard =
      Game.view viewport model

    title =
      drawTitle viewport

    gameState =
      Game.gameState model
      |> fromString
      |> leftAligned

    currentPlayer =
      "Current Turn: " ++ Game.playerName model.currentPlayer
      |> fromString
      |> leftAligned

    infoSpacer =
      spacer 10 10

    boardInfo =
      flow down [ title
      , infoSpacer
      , gameState
      , infoSpacer
      , currentPlayer
      , infoSpacer
      , show mousePosition
      ]

  in
    layers [ boardInfo
    , gameBoard
    ]

drawTitle : Viewport -> Element
drawTitle viewport =
  "Tic\nTac\nToe"
  |> fromString
  |> leftAligned

