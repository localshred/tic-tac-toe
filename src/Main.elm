module Main where

import Game
import Graphics.Element exposing (Element, layers, show)
import Mouse
import Window

main : Signal Element
main =
  Signal.map3 view Mouse.position Window.dimensions nextGameState

mouseDownSampling : Signal (Int, Int)
mouseDownSampling =
  Signal.sampleOn Mouse.isDown Mouse.position

nextGameState : Signal Game.Model
nextGameState =
  Signal.foldp Game.update Game.init mouseDownSampling

view : (Int,Int) -> (Int,Int) -> Game.Model -> Element
view mousePosition dimensions model =
  let
    gameBoard = Game.view dimensions model
  in
    layers [ show mousePosition
    , gameBoard
    ]
