module Model exposing (..)
import Report.Model as Report
import Report.Model exposing (..)
import Page.Model exposing (..)
import Prime exposing (..)
import NList exposing (..)
import Koncepts.Model exposing (..)

type SelectedItem =
   SelectedAbstract AbstractKoncept
   | SelectedValue ValueKoncept
   | SelectedPage Page
   | SelectedCell (ValueKoncept, List Member)

type alias ReportModel =
   {
            report: Report
         ,  selectedItem: Maybe SelectedItem
   }

type Model =
   NoReportWithError String
   | ReportWithError (String, ReportModel)
   | ReportWithoutError ReportModel

addError: Maybe Model -> String -> Model
addError model err =
   let 
      f: Model -> String -> Model
      f m e =
         case m of
            NoReportWithError _ -> NoReportWithError e
            ReportWithError (_,r) -> ReportWithError (err,r)
            ReportWithoutError r -> ReportWithoutError r
   in
      case model of
         Just rm -> f rm err
         Nothing -> NoReportWithError err

-- Result String Report -> Model -> Result String Model

selectItem: SelectedItem -> Model -> Model 
selectItem item model = 
   case model of
      NoReportWithError err -> NoReportWithError err
      ReportWithError (err, reportModel) ->  
         ReportWithoutError { reportModel | selectedItem = Just item}
      ReportWithoutError reportModel -> 
         ReportWithoutError { reportModel | selectedItem = Just item}


tryGetReportModel: Model -> Maybe ReportModel
tryGetReportModel model =
   case model of
      NoReportWithError _ -> Nothing
      ReportWithError (_,rm) -> Just rm
      ReportWithoutError rm -> Just rm

tryGetSelectedCellFromReportModel: ReportModel -> Maybe (ValueKoncept, List Member)
tryGetSelectedCellFromReportModel model =
            case model.selectedItem of
               Just item ->
                  case item of
                     SelectedCell (v,m) -> Just (v,m)
                     _ -> Nothing
               Nothing -> Nothing


tryGetSelectedCell: Model -> Maybe (ValueKoncept, List Member)
tryGetSelectedCell  =
   tryGetReportModel 
   >> Maybe.andThen tryGetSelectedCellFromReportModel
   


update: Result String Report -> Maybe Model -> Model
update result model =
   case result of
      Ok report -> ReportWithoutError { report = report , selectedItem = Nothing}
      Err err -> addError model err 


   
