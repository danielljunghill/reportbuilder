module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Platform.Cmd

import Report.Model exposing (..)

import Report.View as Report
import Report.Mock as Report 
import Koncepts.Model exposing (..)
import Koncepts.Koncept exposing (..)
import Model 
import Model exposing (..)
import Msg exposing (..)
import ResultExtension exposing (..)


main : Program () Model Msg
main =
  Browser.element
     {   
            init = \flags -> (initialModel, Cmd.none)
          , update = update
          , view = view
          , subscriptions = \_ -> Sub.none }

initialModel: Model.Model
initialModel = Nothing |> Model.update Report.mockReport 


update : Msg -> Model.Model  -> (Model.Model, Cmd Msg) 
update msg model =
   case msg of
      SelectMsg selection ->
         model
         |> Model.select selection 
         |> (\m -> (m, Cmd.none))

--   case msg of
--     Select ki -> 
--         let
--           id = Debug.log "Id " ki.id
--           --newModel = report.pages.se.koncepts |> List.map (Koncept.select ki) |> Model.toModel
--         in
--           report


--     SelectPage page -> 
--        page |> Report.selectPage report
--     Add -> report
--     AddPage -> Report.addNewPage reportHuva


-- konceptButton:  Report -> Html Msg
-- konceptButton  _ = 
--   button [ Add |> onClick ] [ text "add Koncept" ] 

-- pageButton:  Html Msg
-- pageButton  = 
--   button [ AddPage |> onClick ] [ text "add Page" ] 

view : Model.Model -> Html Msg
view model =
   let
      modelToHtml: Report -> Html Msg
      modelToHtml report =
           div [ class "report-wrapper"]
            [
            Report.toHtml (tryGetSelection model) report,
            div [] [ ]
            ]
   in
      case model of
         NoReportWithError err -> div [ class "report-wrapper"] [ text err]
         ReportWithError (_, rm) -> modelToHtml rm.report
         ReportWithoutError rm -> modelToHtml rm.report
    
  
  