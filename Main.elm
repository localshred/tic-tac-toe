module Main where

import Html exposing (Html, div, text, h1, button, p, span, a)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList, value, id, href, style)
import StartApp.Simple as StartApp
import String

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
              newStateFromUpdatedSelections nextSelections model.state model.currentPlayer

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

newStateFromUpdatedSelections : List Selection -> GameState -> Player -> GameState
newStateFromUpdatedSelections selections state player =
  if didPlayerWin player selections then
    Winner player

  else if isStalemate selections then
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

didPlayerWin : Player -> List Selection -> Bool
didPlayerWin player selections =
  let
    -- didPlayerWinCombination : (Selection,Selection,Selection) -> Bool
    didPlayerWinCombination combination =
      case combination of
        (node1,node2,node3) ->
          let
            nodes = ( List.member node1 selections
            , List.member node2 selections
            , List.member node3 selections
            )
          in
            case nodes of
              (True, True, True) ->
                True

              otherwise ->
                False

    combinations =
      winningCombinations player
  in
    List.any didPlayerWinCombination combinations

view : Signal.Address Action -> Model -> Html
view address model =
  let
    title =
      h1 [ class "title" ] [ text "Tic Tac Toe" ]

    squaresRow1 =
      row [ squareBuilder address model 0 0
      , squareBuilder address model 0 1
      , squareBuilder address model 0 2
      ]

    squaresRow2 =
      row [ squareBuilder address model 1 0
      , squareBuilder address model 1 1
      , squareBuilder address model 1 2
      ]

    squaresRow3 =
      row [ squareBuilder address model 2 0
      , squareBuilder address model 2 1
      , squareBuilder address model 2 2
      ]

    currentGameState =
      div [ class "game-state row-item" ] [ gameState model ]

    restartButton =
      div [ class "restart-button row-item" , style [ ("text-align","center") ] ]
        [ p [] [ button [ onClick address Restart ] [ text "Restart Game" ] ] ]

    attribution =
      div [ class "row-item" , style [ ("text-align","right") ] ]
        [ p [] [ text "Cobbled together by "
        , a [ href "https://twitter.com/localshred" ] [ text "@localshred" ]
        , text " ("
        , a [ href "https://github.com/localshred/tic-tac-toe/" ] [ text "View Source" ]
        , text ")"
        ] ]

    lastRow =
      div [ class "game-controls row" ] [ currentGameState
      , restartButton
      , attribution
      ]

    boardRows =
      title :: [ squaresRow1, squaresRow2, squaresRow3 ] ++ [ lastRow ]

    boardClasses =
      classList [ ("board", True)
      , (boardCssClass model.state, True)
      ]
  in
    div [ boardClasses ] boardRows

boardCssClass : GameState -> String
boardCssClass state =
  case state of
    Winner _ ->
      "winner"

    otherwise ->
      String.toLower <| toString state

row : List Html -> Html
row children =
  div [ class "row" ] children

squareBuilder : Signal.Address Action -> Model -> Row -> Col -> Html
squareBuilder address model row col =
  let
    xIsSelected =
      List.member (X,row,col) model.selections

    oIsSelected =
      List.member (O,row,col) model.selections

    isSelectable =
      (model.state == Pending || model.state == Started)
      && (not xIsSelected || not oIsSelected)

    classes =
      classList [ ("square", True)
      , ("row-item", True)
      , ("selectable", isSelectable)
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

    idAttr =
      id <| "square-" ++ toString row ++ "-" ++ toString col

    attributes =
      idAttr :: classes :: onClickAttribute
  in
    div attributes [ text "" ]

cssClassForPlayer : Player -> String
cssClassForPlayer player =
  "player-" ++ (String.toLower <| toString player)

gameState : Model -> Html
gameState model =
  let
    coloredPlayerName player =
      span [ class <| cssClassForPlayer player ] [ text <| playerName player ]
  in
    case model.state of
      Pending ->
        p [] [ text "Waiting on player "
        , coloredPlayerName model.currentPlayer
        , text " to begin..."
        ]

      Started ->
        p [] [ text "Player "
        , coloredPlayerName model.currentPlayer
        , text ", it is your turn"
        ]

      Winner player ->
        p [] [ text "Player "
        , coloredPlayerName player
        , text " won!"
        ]

      Stalemate ->
        text "Aww shucks, stalemate!"

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
