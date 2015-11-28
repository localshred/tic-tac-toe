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

gutter = 10
rows = 3

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

    infoSpacer =
      spacer 10 10

    boardInfo =
      flow down [ drawTitleView
      , infoSpacer
      , gameStateView model
      , infoSpacer
      , currentPlayerView model
      , infoSpacer
      , show mousePosition
      ]
  in
    layers [ boardInfo
    , gameBoard
    ]

currentPlayerView : Game.Model -> Element
currentPlayerView model =
  "Current Turn: " ++ Game.playerName model.currentPlayer
  |> fromString
  |> leftAligned

drawTitleView : Element
drawTitleView =
  "Tic\nTac\nToe"
  |> fromString
  |> leftAligned

gameStateView : Game.Model -> Element
gameStateView model =
  Game.gameState model
  |> fromString
  |> leftAligned

