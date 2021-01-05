module Section.View exposing (..)


import Msg exposing (..)
import Html exposing (..)
import Section.Model as Model exposing (..)
import Koncept.View as View exposing (..)
import Html.Custom.Classes as Classes exposing (..)
import Msg as Msg exposing (..)
import Html.Attributes exposing (..)

toHtml: Section -> Html Msg            
toHtml section = 
    let
        koncepts: List (Html Msg) 
        koncepts =
            section.koncepts
            |> List.map View.toHtml
    in
        -- List.append [ text section.name ] koncepts
        koncepts
        |> div (Classes.selected section.isSelected |> List.append[ class "section"] )
        --Classes.selected section.isSelected,

    
    