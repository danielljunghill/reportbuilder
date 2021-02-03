module Koncepts.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class)
import Events.Custom exposing (..)
import Koncepts.Model as Koncept
import Koncepts.Model exposing (..)
import Msg exposing (..)
import Msg 

valueKonceptDetails: ValueKoncept -> Html Msg 
valueKonceptDetails vk =
   let

      lbl: Html Msg
      lbl = 
         label 
            [] 
            [  vk.name 
               |> Koncept.valueKonceptNameToString 
               |> text
            ] 
   in
      div (joinAttributes [ class "value-details", onClickStopPropagation ( vk |> SelectItem.Value|> Msg.Select )] (getSelection vk.selected))
            [ 
               lbl ,
               div [ class "value-details-space"] [],
               div [ class "value-details-input"] [  text "input"  ]
            ]


getSelection: Bool -> List (Attribute Msg)
getSelection isSelected = if isSelected then [class "selected"] else []

s
   

joinAttributes: List (Attribute Msg) -> List (Attribute Msg) -> List (Attribute Msg)
joinAttributes a1 a2 = List.append a1 a2


abstractKonceptDetails: AbstractKoncept -> Html Msg 
abstractKonceptDetails ak =

  
  div (joinAttributes [ class "abstract-details", onClickStopPropagation (ak |> SelectItem.Abstract|> Msg.Select ) ] (getSelection ak.selected))
      [ 
          div [ class "abstract-details-label"] [ ak.name |> abstractKonceptNameToString |> text ],
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
    Abstract (ak,kl) ->
        kl 
        |> List.map divKoncept 
        |> divAbstractKoncept ak
    Cube (hc,dims) ->
        div [] [text hc.name]
 


toHtml: Koncept  ->  Html Msg             
toHtml = divKoncept