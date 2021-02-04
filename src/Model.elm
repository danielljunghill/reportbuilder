module Msg exposing (..)
import Report.Model as Report



type ReportModel =
   {
      report:  Report
   }

type Model =
   | NoReportWithError String
   | ReportWithError (string, ReportModel)
   | ReportWithoutError ReportModel

addError: Maybe Model -> String -> Model
addError model err =
   let 
      f: Model -> String -> Model
      f m e =
         case m of
         ErrorNoReport -> ErrorNoReport err
         | ErrorReport (_,r) -> ErrorReport (err,r)
         | NoErrorReport -> NoErrorReport m
   in
      case Maybe.map f model of
         Just rm -> rm
         Noting -> ErrorNoReport err

-- Result String Report -> Model -> Result String Model

update: Result String Report -> Maybe Model -> Model
update result model =
   case result of
      Ok report -> NoErrorReport { report : result}
      Err err -> addError err model





   
