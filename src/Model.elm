module Model exposing (..)
import Report.Model as Report
import Report.Model exposing (..)
import Prime exposing (..)


type alias ReportModel =
   {
      report: Report
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

update: Result String Report -> Maybe Model -> Model
update result model =
   case result of
      Ok report -> ReportWithoutError { report = report }
      Err err -> addError model err 





   
