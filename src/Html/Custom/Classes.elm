module Html.Custom.Classes exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (class)

selected: Bool -> List (Attribute msg)
selected isSelected = if isSelected then [class "selected"] else []



