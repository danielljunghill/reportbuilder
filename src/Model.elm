module Model exposing (..)
import Report.Model as Report
import Report.Model exposing (..)
import Page.Model exposing (..)
import Prime exposing (..)
import NList exposing (..)
import Koncepts.Model exposing (..)


-- type SelectedCellAction =
--    View (ValueKoncept, List Member)
--    | Edit (ValueKoncept, List Member)

-- type SelectedItem =
--    SelectedAbstract AbstractKoncept
--    | SelectedValue ValueKoncept
--    | SelectedPage Page
--    | SelectedCell (ValueKoncept, List Member)




-- change name to editcell
-- and selectCell
-- select column and select 
type Selection =
   EditValue (NList Factor, List AbstractFactor)
   | SelectValue (NList Factor, List AbstractFactor)

factorsInSelection selection =
   case selection of
      EditValue (factors,_) -> factors
      SelectValue (factors,_)-> factors 

abstractFactorsInSelection selection =
      case selection of
         EditValue (_,abstractFactors) -> abstractFactors
         SelectValue (_,abstractFactors) -> abstractFactors 

type UpdateValue = UpdateValue (NList Factor, Content)
updateValue factors content = UpdateValue (factors,content)


type alias ReportModel =
   {
            report: Report
         ,  selection: Maybe Selection
         ,  values: ReportedValues

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

-- TODO fix error handling
-- when to go from WithError to WithoutError
map: (ReportModel -> ReportModel) -> Model -> Model 
map f model = 
   case model of
      NoReportWithError err -> NoReportWithError err
      ReportWithError (err, reportModel) ->  
         ReportWithoutError (f reportModel)
      ReportWithoutError reportModel -> 
         ReportWithoutError (f reportModel)

select: Selection -> Model -> Model 
select newSelection model = 
   case model of
      NoReportWithError err -> NoReportWithError err
      ReportWithError (err, reportModel) ->  
         ReportWithoutError { reportModel | selection = Just newSelection}
      ReportWithoutError reportModel -> 
         ReportWithoutError { reportModel | selection = Just newSelection}



tryGetReportModel: Model -> Maybe ReportModel
tryGetReportModel model =
   case model of
      NoReportWithError _ -> Nothing
      ReportWithError (_,rm) -> Just rm
      ReportWithoutError rm -> Just rm



init: Result String Report -> Model
init result =
   case result of
      Ok report -> ReportWithoutError { report = report , selection = Nothing , values = emptyReportedValues}
      Err err -> NoReportWithError err


   
