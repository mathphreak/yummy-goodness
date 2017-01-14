module HtmlExtra exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode as Encode


link =
    node "link"


script =
    node "script"


integrity =
    property "integrity" << Encode.string


crossorigin =
    property "crossorigin" << Encode.string


onclick =
    attribute "onclick"


onload =
    attribute "onload"
