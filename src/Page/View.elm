module Page.View exposing (..)
import Page.Model exposing (..)
import Section.View as Sections exposing (..) 
import Html.Custom.Classes as Classes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Msg as Msg exposing (..)
import Html.Events exposing (..)

toHtml: Page ->  Html Msg            
toHtml page = 
    let
        sections: List (Html Msg) 
        sections =
            page.sections
            |> List.map Sections.toHtml
    in

        -- List.append [ text page.name] sections
        sections
        |> div [class "page"]
        
toHeaderHtlm: Page -> Html Msg
toHeaderHtlm page =
    div ([class "header", onClick (SelectPage page)] ++ (Classes.selected page.selected)) [ text page.name ]
 

       -- Classes.selected page.isSelected