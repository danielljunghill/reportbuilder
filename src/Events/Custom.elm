module Events.Custom exposing (onClickStopPropagation)

import Html.Events exposing (..)
import Html exposing (Attribute)
import Html exposing (Attribute)
import Json.Decode as Json

alwaysStop : a -> (a, Bool)
alwaysStop = (\x -> (x, True))
noPropapagation: String -> msg -> Attribute msg
noPropapagation eventId msg =
    stopPropagationOn eventId  (Json.map alwaysStop (Json.succeed msg))

onClickStopPropagation:msg -> Attribute msg
onClickStopPropagation  =
    (\msg -> noPropapagation "click" msg)   