module Koncept.View exposing (toHtml)


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)
import Html.Attributes exposing (classList)
import Html.Attributes exposing (class)
import Events.Custom exposing (..)
import Koncept.Model as Koncept exposing (..)
import Msg as Msg exposing (..)



valueKonceptDetails: ValueKoncept -> Html Msg 
valueKonceptDetails (ValueKoncept ki) =
  div (joinAttributes [ class "value-details", onClickStopPropagation (Select ki)] (getSelection ki))
      [ 
          div [ class "value-details-label"] [ text ki.name ],
          div [ class "value-details-space"] [],
          div [ class "value-details-input"] [ button [] [ text "input" ] ]
      ]


getSelection: KonceptInformation -> List (Attribute Msg)
getSelection ki = if ki.selected then [class "selected"] else []

joinAttributes: List (Attribute Msg) -> List (Attribute Msg) -> List (Attribute Msg)
joinAttributes a1 a2 = List.append a1 a2


abstractKonceptDetails: AbstractKoncept -> Html Msg 
abstractKonceptDetails (AbstractKoncept ki) =
  div (joinAttributes [ class "abstract-details", onClickStopPropagation (Select ki) ] (getSelection ki))
      [ 
          div [ class "abstract-details-label"] [ text ki.name ],
          div [ class "abstract-details-space"] [],
          div [ class "abstract-details-description"] [ text "abstract description"]
      ]

divAbstractKoncept:  AbstractKoncept -> List (Html Msg) -> Html Msg
divAbstractKoncept ak htmls = 
    List.append [ abstractKonceptDetails ak] htmls  
    |> div  [class "koncept", class "abstract"] 

divValueKoncept: ValueKoncept -> Html Msg
divValueKoncept vk =
      div [class "koncept", class "value"] 
       [ valueKonceptDetails vk ]

divKoncept : Koncept ->  Html Msg
divKoncept koncept =
  case koncept of
    Value vk ->
        divValueKoncept vk
    Koncepts (ak,kl) ->
        kl 
        |> List.map divKoncept 
        |> divAbstractKoncept ak
 


toHtml: Koncept  ->  Html Msg             
toHtml = divKoncept