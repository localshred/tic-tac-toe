module UI (..) where

import Html exposing (Html, Attribute, button, text)
import Html.Attributes exposing (class)

pureButton : Attribute -> String -> Html
pureButton onClickHandler buttonText =
  button [ class "pure-button", onClickHandler ] [ text buttonText ]

