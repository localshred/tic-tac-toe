module UI (..) where

import Html exposing (..)
import Html.Attributes exposing (..)

pureButton : Attribute -> String -> Html
pureButton onClickHandler buttonText =
  button [ class "pure-button", onClickHandler ] [ text buttonText ]

