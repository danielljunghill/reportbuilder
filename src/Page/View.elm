module Page.View exposing (..)
import Page.Model exposing (..)
import Html.Custom.Classes as Classes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.View as KonceptView
import Msg exposing (..)
import Model exposing  (..)

toHtml: Maybe Selection -> Page ->  Html Msg            
toHtml selection page = 
    let
        koncepts: List (Html Msg) 
        koncepts =
            page.koncepts
            |> List.map (KonceptView.toHtml selection)
    in

        -- List.append [ text page.name] sections
        koncepts
        |> div [class "page"]
        
toHeaderHtlm: Page -> Html Msg
toHeaderHtlm page =
    -- div ([class "header", onClick ( page |>  Msg.selectPage )] ++ (Classes.selected page.selected)) [ text page.name ]
    div ([class "header"] ++ (Classes.selected page.selected)) [ text page.name ]

       -- Classes.selected page.isSelected