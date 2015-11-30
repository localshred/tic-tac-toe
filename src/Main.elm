module Main where

import Html exposing (Html, div, text, h1, button, p, span, a, header, footer, nav)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList, value, id, style, href)
import StartApp.Simple as StartApp
import Games.TicTacToe as TicTacToe
import UI

type GameType =
  NoGame
  | TicTacToe

type Action =
  ChangeGame GameType
  | TicTacToeEvent TicTacToe.Action

type alias Model =
  { selectedGame : GameType
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
  Model NoGame TicTacToe.init

update : Action ->  Model -> Model
update action model =
  case action of
    ChangeGame gameType ->
      { model | selectedGame = gameType }

    TicTacToeEvent action ->
      { model | ticTacToeModel = TicTacToe.update action model.ticTacToeModel }

view : Signal.Address Action -> Model -> Html
view address model =
  let
    gameWrapperClasses =
      classList [ ("game", True)
      , ("game-selector", model.selectedGame == NoGame)
      , ("tic-tac-toe", model.selectedGame == TicTacToe)
      ]

    gameView =
      case model.selectedGame of
        NoGame ->
          noGameView

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
    ticTacToeButton =
      UI.pureButton (onClick address (ChangeGame TicTacToe)) "Tic-Tac-Toe"
  in
    nav [ class "game-selector-menu" ] [ ticTacToeButton
    ]

noGameView : Html
noGameView =
  div [ class "no-game" ] [ text "Select your game above to get started!" ]
