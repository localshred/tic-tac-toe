module Main where

import Effects exposing (Effects)
import Games.Minesweeper as Minesweeper
import Games.TicTacToe as TicTacToe
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Signal
import UI

type GameType =
  NoGame
  | Minesweeper
  | TicTacToe

type Action =
  ChangeGame GameType
  | MinesweeperEvent Minesweeper.Action
  | TicTacToeEvent TicTacToe.Action

type alias Model =
  { selectedGame : GameType
  , minesweeperModel : Minesweeper.Model
  , ticTacToeModel : TicTacToe.Model
  }

main : Signal Html
main =
  app.html

app : StartApp.App Model
app =
  StartApp.start { init = init
  , update = update
  , view = view
  , inputs = inputs
  }

inputs : List (Signal Action)
inputs =
  [ Signal.map MinesweeperEvent Minesweeper.inputs
  ]

init : (Model, Effects Action)
init =
  let
    model =
      Model NoGame Minesweeper.init TicTacToe.init
      |> update (ChangeGame Minesweeper) -- FIXME remove this to get back to game selector screen
      |> fst
  in
    (,) model Effects.none

update : Action ->  Model -> (Model, Effects Action)
update action model =
  let
    model' =
      case action of
        ChangeGame gameType ->
          { model | selectedGame = gameType }

        MinesweeperEvent action ->
          { model | minesweeperModel = Minesweeper.update action model.minesweeperModel }

        TicTacToeEvent action ->
          { model | ticTacToeModel = TicTacToe.update action model.ticTacToeModel }
  in
    (,) model' Effects.none

view : Signal.Address Action -> Model -> Html
view address model =
  let
    gameWrapperClasses =
      classList [ ("game", True)
      , ("game-selector", model.selectedGame == NoGame)
      , ("minesweeper", model.selectedGame == Minesweeper)
      , ("tic-tac-toe", model.selectedGame == TicTacToe)
      ]

    gameView =
      case model.selectedGame of
        NoGame ->
          noGameView

        Minesweeper ->
          Minesweeper.view (Signal.forwardTo address MinesweeperEvent) model.minesweeperModel

        TicTacToe ->
          TicTacToe.view (Signal.forwardTo address TicTacToeEvent) model.ticTacToeModel
  in
    div [] [
      header [] [
        h1 [ class "title" ] [
          text "@localshred Gaming, Inc."
        ]
      , gameSelectorMenu address
      ]
      , div [ gameWrapperClasses ] [
        gameView
      ]
      , footer [] [
        p [] [ text "Cobbled together by "
        , a [ href "https://twitter.com/localshred" ] [ text "@localshred" ]
        , text " ("
        , a [ href "https://github.com/localshred/tic-tac-toe/" ] [ text "View Source" ]
        , text ")"
        ]
      ]
    ]

gameSelectorMenu : Signal.Address Action -> Html
gameSelectorMenu address =
  let
    minesweeperButton =
      UI.pureButton (onClick address (ChangeGame Minesweeper)) "Minesweeper"

    ticTacToeButton =
      UI.pureButton (onClick address (ChangeGame TicTacToe)) "Tic-Tac-Toe"
  in
    nav [ class "game-selector-menu" ] [
      minesweeperButton
      , ticTacToeButton
    ]

noGameView : Html
noGameView =
  div [ class "no-game" ] [ text "Select your game above to get started!" ]
