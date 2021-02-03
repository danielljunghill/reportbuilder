module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)

import Report.Model as Model exposing (..)
import Report.View as View exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.Koncept exposing (..)
import Koncepts.Mock as Mock
import Msg exposing (..)

import ResultExtension exposing (..)
main : Program () Model.Report Msg
main =
  Browser.sandbox { init = init, update = update, view = view }

init: Model.Report
init = Mock.report

update : Msg -> Model.Report -> Model.Report 
update msg report =
  case msg of
    Select ki -> 
        let
          id = Debug.log "Id " ki.id
          --newModel = report.pages.se.koncepts |> List.map (Koncept.select ki) |> Model.toModel
        in
          report


    SelectPage page -> 
       page |> Model.selectPage report
    Add -> report
    AddPage -> Model.addNewPage report


konceptButton:  Model.Report -> Html Msg
konceptButton  htmls = 
  button [ Add |> onClick ] [ text "add Koncept" ] 

pageButton:  Html Msg
pageButton  = 
  button [ AddPage |> onClick ] [ text "add Page" ] 
    

view : Model.Report -> Html Msg
view report =
    div [ class "report-wrapper"]
      [
        View.toHtml report,
        div [] [ konceptButton report,  pageButton]
      ]
  
  