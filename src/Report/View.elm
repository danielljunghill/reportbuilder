module Report.View exposing (..)

import Html.Custom.Classes as CS exposing (..)
import Page.View as Pages exposing (..)
import Msg as Msg exposing (..)
import Html exposing (..)
import Msg exposing (..)
import Report.Model exposing (..)
import Html.Attributes exposing(..)

toHtml: Report -> Html Msg            
toHtml report = 
    let
        pages: List (Html Msg) 
        pages =
            report.pages
            |> List.filter (\page -> page.selected)
            |> List.map Pages.toHtml

        headers: List (Html Msg)
        headers =
            report.pages
            |> List.map Pages.toHeaderHtlm

        pagesHtml : List (Html Msg)
        pagesHtml =
            [
                div [class "pages"] pages,
                div [class "headers"] headers
            ]
    in

        List.append [ text report.name ] pagesHtml
        |> div [class "Report"] 
