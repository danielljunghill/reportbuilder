module Koncepts.View exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (class)
import Events.Custom exposing (..)
import Koncepts.Model as Koncept
import Koncepts.Model exposing (..)
import Msg 
import Msg exposing (..)
import Koncepts.CubeView exposing (..)
import Koncepts.Area exposing (..)
import Model exposing (..)


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
        -- div (joinAttributes [ class "value-details", onClickStopPropagation ( vk |> Msg.selectValue)] (getSelection vk.selected))
      div (joinAttributes [ class "value-details"] (getSelection vk.selected))
            [ 
               lbl ,
               div [ class "value-details-space"] [],
               div [ class "value-details-input"] [  text "input"  ]
            ]


getSelection: Bool -> List (Attribute Msg)
getSelection isSelected = if isSelected then [class "selected"] else []


   

joinAttributes: List (Attribute Msg) -> List (Attribute Msg) -> List (Attribute Msg)
joinAttributes a1 a2 = List.append a1 a2


abstractKonceptDetails: AbstractKoncept -> Html Msg 
abstractKonceptDetails ak =
  
--   div (joinAttributes [ class "abstract-details", onClickStopPropagation (ak |> Msg.selectAbstract ) ] (getSelection ak.selected))
  
  div (joinAttributes [ class "abstract-details" ] (getSelection ak.selected))
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

divKoncept : Maybe Selection -> Koncept -> Html Msg
divKoncept selection koncept    =
  case koncept of
    Value vk ->
        divValueKoncept vk
    Abstract (ak,kl) ->
        kl 
        |> List.map (divKoncept selection)
        |> divAbstractKoncept ak
    Cube (hc,dk) ->
        div [] 
            [ 
                    hc.name |> Koncept.hyperCubeNameToString |> text
                ,   viewCube Horizontal hc dk selection
            ]
 


toHtml: Maybe Selection -> Koncept  ->  Html Msg             
toHtml selection  = divKoncept selection