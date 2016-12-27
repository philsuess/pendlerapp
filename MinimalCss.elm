module Main exposing (..)

import Html exposing (h1, text)
import Html.Attributes exposing (..)
import Css exposing (..)
import Css.Elements exposing (body)


style : List Css.Mixin -> Html.Attribute msg
style =
    Css.asPairs >> Html.Attributes.style


title : Html.Attribute msg
title =
    style
        [ backgroundColor (rgb 0 0 0)
        , color (rgb 255 255 255)
        ]

main : Html.Html msg
main =
    h1 [ title ] [ Html.text "Hello World" ]
