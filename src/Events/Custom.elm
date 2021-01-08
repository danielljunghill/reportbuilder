module Events.Custom exposing (onClickStopPropagation)

import Html.Events exposing (..)
import Html exposing (Attribute)
import Msg as Msg exposing (..)
import Html exposing (Attribute)
import Json.Decode as Json

alwaysStop : a -> (a, Bool)
alwaysStop = (\x -> (x, True))
noPropapagation: String -> Msg -> Attribute Msg
noPropapagation eventId msg =
    stopPropagationOn eventId  (Json.map alwaysStop (Json.succeed msg))

onClickStopPropagation:Msg -> Attribute Msg
onClickStopPropagation  =
    (\msg -> noPropapagation "click" msg)   