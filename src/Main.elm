module Main where

import Html exposing (Html, div, text, h1, p)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList)
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
  let
    selections =
      case action of
        SelectSquare selection ->
          selection :: model.selections
  in
    { model |
      currentPlayer = nextPlayer model.currentPlayer
      , state = Started
      , selections = selections
    }

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
    div [] [ infoPanel model
    , div [ class "board" ] [ row1, row2, row3 ]
    ]

infoPanel : Model -> Html
infoPanel model =
  let
    title =
      h1 [ class "title" ] [ text "Tic Tac Toe" ]

    currentGameState =
      p [ class "game-state" ] [ text <| "Status: " ++ gameState model ]

    currentPlayer =
      p [ class "current-player" ] [ text <| "Current Player: " ++ playerName model.currentPlayer ]
  in
    div [ class "info-panel" ] [ title
    , currentGameState
    , currentPlayer
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

    allowOnClick =
      (not (model.state == Pending) && not (model.state == Started))
      || xIsSelected
      || oIsSelected

    attributes =
      if allowOnClick then
        [ classes ]

      else
        [ classes
        , onClick address <| SelectSquare (model.currentPlayer,row,col)
        ]
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
