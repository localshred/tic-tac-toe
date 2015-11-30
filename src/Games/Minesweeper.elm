module Games.Minesweeper (Action, Model, view, update, init) where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String
import UI

type alias Row =
  Int

type alias Col =
  Int

type alias SquarePos =
  (Row,Col)

type Item =
  Nothing
  | Flag
  | Bomb

type Visibility =
  Hidden
  | Peeking
  | Visible

type alias Square =
  (SquarePos,Item,Visibility)

type GameState =
  Pending
  | Started
  | Win
  | Loss

type Action =
  Restart
  | SelectSquare SquarePos
  | PeekSquare SquarePos

type alias Model =
  { size : Int
  , squares : List Square
  , state : GameState
  }

init : Model
init =
  Model 10 [] Pending

view : Signal.Address Action -> Model -> Html
view address model =
  div [] [ text "Play ball!" ]

update : Action -> Model -> Model
update action model =
  model

