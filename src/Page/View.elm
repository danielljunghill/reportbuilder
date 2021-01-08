module Page.View exposing (..)
import Page.Model exposing (..)
import Koncept.View as Koncepts exposing (..) 
import Html.Custom.Classes as Classes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msg as Msg exposing (..)
import Html.Events exposing (..)
import Koncept.Model exposing (Koncept)

toHtml: Page ->  Html Msg            
toHtml page = 
    let
        koncepts: List (Html Msg) 
        koncepts =
            page.koncepts
            |> List.map Koncepts.toHtml
    in

        -- List.append [ text page.name] sections
        koncepts
        |> div [class "page"]
        
toHeaderHtlm: Page -> Html Msg
toHeaderHtlm page =
    div ([class "header", onClick (SelectPage page)] ++ (Classes.selected page.selected)) [ text page.name ]
 

       -- Classes.selected page.isSelected