module Main where

import Html exposing (Html, div, text, h1, p, button)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList, value)
import StartApp.Simple as StartApp

type Player =
  X
  | O

type alias Row =
  Int

type alias Col =
  Int

type Action =
  SelectSquare Selection
  | Restart

type GameState =
  Pending
  | Started
  | Winner Player
  | Stalemate

type alias Selection =
  (Player,Row,Col)

type alias Model =
  { selections : List Selection
  , state : GameState
  , currentPlayer : Player
  }

main : Signal Html
main =
  { model = init
  , update = update
  , view = view
  } |> StartApp.start

init : Model
init =
  Model [] Pending X

update : Action ->  Model -> Model
update action model =
  case action of
    SelectSquare selection ->
      let
        nextSelections =
          selection :: model.selections

        nextState =
          case model.state of
            Pending ->
              Started

            Started ->
              newStateFromUpdatedSelections nextSelections model.state

            otherwise ->
              model.state
      in
        { model |
          currentPlayer = nextPlayer model.currentPlayer
          , state = nextState
          , selections = nextSelections
        }

    Restart ->
      init

isStalemate : List Selection -> Bool
isStalemate selections =
  List.length selections == 9

newStateFromUpdatedSelections : List Selection -> GameState -> GameState
newStateFromUpdatedSelections selections state =
  if isStalemate selections then
    Stalemate

  else
    state

winningCombinations : Player -> List (Selection,Selection,Selection)
winningCombinations p =
  [ ((p,0,0),(p,0,1),(p,0,2))
  , ((p,1,0),(p,1,1),(p,1,2))
  , ((p,2,0),(p,2,1),(p,2,2))
  , ((p,0,0),(p,1,1),(p,2,2))
  , ((p,0,2),(p,1,1),(p,2,0))
  , ((p,0,0),(p,1,0),(p,2,0))
  , ((p,0,1),(p,1,1),(p,2,1))
  , ((p,0,2),(p,1,2),(p,2,2))
  ]

didXWin : List Selection -> Bool
didXWin selections =
  let
    combinations =
      winningCombinations X
  in
    True


view : Signal.Address Action -> Model -> Html
view address model =
  let
    row1 =
      div [ class "row" ] [ squareBuilder address model 0 0
                          , squareBuilder address model 0 1
                          , squareBuilder address model 0 2
                          ]

    row2 =
      div [ class "row" ] [ squareBuilder address model 1 0
                          , squareBuilder address model 1 1
                          , squareBuilder address model 1 2
                          ]

    row3 =
      div [ class "row" ] [ squareBuilder address model 2 0
                          , squareBuilder address model 2 1
                          , squareBuilder address model 2 2
                          ]

  in
    div [] [ infoPanel address model
    , div [ class "board" ] [ row1, row2, row3 ]
    ]

infoPanel : Signal.Address Action -> Model -> Html
infoPanel address model =
  let
    title =
      h1 [ class "title" ] [ text "Tic Tac Toe" ]

    currentGameState =
      p [ class "game-state" ] [ text <| "Status: " ++ gameState model ]

    currentPlayer =
      p [ class "current-player" ] [ text <| "Current Player: " ++ playerName model.currentPlayer ]

    restartButton =
      button [ onClick address Restart ] [ text "Restart" ]
  in
    div [ class "info-panel" ] [ title
    , currentGameState
    , currentPlayer
    , restartButton
    ]

squareBuilder : Signal.Address Action -> Model -> Row -> Col -> Html
squareBuilder address model row col =
  let
    xIsSelected =
      List.member (X,row,col) model.selections

    oIsSelected =
      List.member (O,row,col) model.selections

    classes =
      classList [ ("square", True)
      , ("selectable", not xIsSelected && not oIsSelected)
      , ("selected", xIsSelected || oIsSelected)
      , ("player-x", xIsSelected)
      , ("player-o", oIsSelected)
      ]

    disallowOnClick =
      (not (model.state == Pending) && not (model.state == Started))
      || xIsSelected
      || oIsSelected

    onClickAttribute =
      if not disallowOnClick then
        [ onClick address <| SelectSquare (model.currentPlayer,row,col) ]

      else
        []

    attributes =
      classes :: onClickAttribute
  in
    div attributes [ text "" ]

gameState : Model -> String
gameState model =
  case model.state of
    Pending ->
      "Waiting for player " ++ playerName model.currentPlayer ++ " to begin..."

    Started ->
      "Started"

    Winner player ->
      "Player " ++ playerName player ++ " won!"

    Stalemate ->
      "Aww shucks, stalemate!"

nextPlayer : Player -> Player
nextPlayer player =
  case player of
    X ->
      O

    O ->
      X

playerName : Player -> String
playerName player =
  case player of
    X ->
      "X"

    O ->
      "O"
