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
initialModel = Model.init Report.mockReport 

updateValueStore: UpdateValue -> ReportModel -> ReportModel 
updateValueStore (UpdateValue (factors,content)) reportModel =
      factors
      |> multiplyFactors
      |> ReportedValueFactor
      |> insertReportedValue reportModel.values (ReportedValue content) 
      |> (\values -> { reportModel | values = values})


--insertReportedValue: ReportedValueFactor -> ReportedValue -> ReportedValues -> ReportedValues

update : Msg -> Model.Model  -> (Model.Model, Cmd Msg) 
update msg model =
   case msg of
      SelectMsg selection ->
         model
         |> Model.select selection 
         |> (\m -> (m, Cmd.none))
      UpdateValueMsg updatedValue ->
         model
         |> Model.map (updateValueStore updatedValue)
         |> (\newModel ->  (newModel,Cmd.none))


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
      modelToHtml: ReportModel -> Html Msg
      modelToHtml reportModel =
            let 
               valueFecther = 
                  reportModel.values 
                  |> getReportedValue
                  |> ValueFetcher  
               in

            div [ class "report-wrapper"]
               [
               Report.toHtml valueFecther reportModel.selection reportModel.report,
               div [] [ ]
               ]
   in
      case model of
         NoReportWithError err -> div [ class "report-wrapper"] [ text err]
         ReportWithError (_, rm) -> modelToHtml rm
         ReportWithoutError rm -> modelToHtml rm
    
  
  