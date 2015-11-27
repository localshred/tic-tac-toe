module Main where

import Game
import Graphics.Element exposing (Element)
import Mouse
import Window

main : Signal Element
main =
  Signal.map2 Game.view Window.dimensions nextGameState

mouseDownSampling : Signal (Int, Int)
mouseDownSampling =
  Signal.sampleOn Mouse.isDown Mouse.position

nextGameState : Signal Game.Model
nextGameState =
  Signal.foldp Game.update Game.init mouseDownSampling
