module Report.View exposing (..)

import Html.Custom.Classes exposing (..)
import Page.View as Pages exposing (..)
import Html exposing (..)
import Msg exposing (..)
import Report.Model exposing (..)
import Html.Attributes exposing(..)
import List
import Koncepts.Model exposing(..)
import Model exposing (..)

toHtml: ValueFetcher -> Maybe Selection -> Report -> Html Msg            
toHtml valueFetcher selection report = 
    let
        pages: List (Html Msg) 
        pages =
            report.pages
            |> List.filter (\page -> page.selected)
            |> List.map (Pages.toHtml valueFetcher selection)

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