module Game (
  gameState
  , init
  , Model
  , playerName
  , update
  , view
  ) where

import Color exposing (blue, green, red)
import Graphics.Collage exposing (Form, toForm, collage, moveY, move, outlined, filled, dashed, square, circle, solid, scale)
import Graphics.Element exposing (Element, rightAligned, leftAligned, show)

type Player =
  X | O

type State =
  Pending
  | Started
  | Winner Player
  | Stalemate

type alias Row =
  Int

type alias Col =
  Int

type alias Selection =
  (Row,Col,Player)

type alias Model =
  { selections : List Selection
  , state : State
  , currentPlayer : Player
  , points : List (Float,Float)
  }

cols = 3
rows = 3


gameState : Model -> String
gameState model =
    case model.state of
        Pending ->
            "Pending"

        Started ->
            "Started"

        Winner player ->
            "Player " ++ playerName player ++ " won!"

        Stalemate ->
            "Aww shucks, stalemate!"

init : Model
init =
  Model [] Pending X []

isPointInSquare : Viewport -> (Int,Int) -> Row -> Col -> Bool
isPointInSquare viewport (x,y) row col =
  True -- TODO write this thing

nextPlayer : Player -> Player
nextPlayer player =
  case player of
    X ->
      O

    O ->
      X

makeRows : Model -> Viewport -> List Form
makeRows model viewport =
  let
    rowSquareMaker color =
      makeSquare model viewport color

    makeFirstRowSquare n =
      rowSquareMaker green 0 n

    makeSecondRowSquare n =
      rowSquareMaker blue 1 n

    makeThirdRowSquare n =
      rowSquareMaker red 2 n
  in
    [ makeFirstRowSquare 0
    , makeFirstRowSquare 1
    , makeFirstRowSquare 2
    , makeSecondRowSquare 0
    , makeSecondRowSquare 1
    , makeSecondRowSquare 2
    , makeThirdRowSquare 0
    , makeThirdRowSquare 1
    , makeThirdRowSquare 2
    ]

makeSquare : Model -> Viewport -> Color.Color -> Int -> Int -> Form
makeSquare model viewport color row col  =
  let
    coordinates =
      squarePosition row col viewport
  in
    outlined (dashed color) (square viewport.squareWidth)
    |> move coordinates

playerName : Player -> String
playerName player =
  case player of
    X ->
      "X"

    O ->
      "O"

offset : Int -> Viewport -> Float
offset pos viewport =
  let
    leftRight =
      case pos of
        0 ->
          1

        1 ->
          0

        2 ->
          -1

        otherwise ->
          0

    operands =
      [ (viewport.halfBoardWidth * -1)
      , ((toFloat pos) * viewport.halfBoardWidth)
      , (viewport.halfSquareWidth * leftRight)
      ]
  in
    List.foldr (+) 0 operands

squarePosition : Int -> Int -> Viewport -> (Float,Float)
squarePosition row col viewport =
  let
    x =
      offset col viewport

    y =
      offset row viewport
  in
    (,) x y

update : (Int,Int) -> Model -> Model
update (x,y) previousModel  =
  let
    filteredPoints =
      List.filter (\point -> point /= (0,0)) previousModel.points
  in
    { previousModel |
      currentPlayer = nextPlayer previousModel.currentPlayer
      , state = Started
      , points = (toFloat x, toFloat y) :: filteredPoints
    }

view : Viewport -> Model -> Element
view viewport model =
  let
    gameRows =
      makeRows model viewport

    drawCanvas forms =
      collage viewport.width viewport.height forms
  in
    drawCanvas gameRows

