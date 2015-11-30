module Main where

import Games.Minesweeper as Minesweeper
import Games.TicTacToe as TicTacToe
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp
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
  { model = init
  , update = update
  , view = view
  } |> StartApp.start

init : Model
init =
  Model NoGame Minesweeper.init TicTacToe.init

update : Action ->  Model -> Model
update action model =
  case action of
    ChangeGame gameType ->
      { model | selectedGame = gameType }

    MinesweeperEvent action ->
      { model | minesweeperModel = Minesweeper.update action model.minesweeperModel }

    TicTacToeEvent action ->
      { model | ticTacToeModel = TicTacToe.update action model.ticTacToeModel }

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
