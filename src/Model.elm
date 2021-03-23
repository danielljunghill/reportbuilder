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

type Content = Content String

type Edited =
   EditValue (ValueKoncept,Content)
   | EditValueMember (ValueKoncept,NList Member, Content)

type Selected =
   SelectValue ValueKoncept
   | SelectValueMember (ValueKoncept,NList Member)
   | SelectMember (NList Member)

type Selection =
   Select Selected
   | Edit Edited

membersFactors: NList Member -> List Factor
membersFactors members =
   members
   |> NList.toList
   |> List.map (\m -> m.factor)

selectedFactors: Selected -> NList Factor
selectedFactors selected =
   case selected of
      SelectValue vk -> NList.create vk.factor 
      SelectValueMember (vk, members) -> 
         NList.create2 vk.factor (membersFactors members)
      SelectMember members ->  members |> NList.map (\m -> m.factor)

editedFactors edited =
   case edited of
      EditValue (vk,_) -> 
         NList.create vk.factor
      EditValueMember (vk,members, _) -> 
         NList.create2 vk.factor (membersFactors members)

selectionFactors selection =
   case selection of
       Select selected -> selectedFactors selected
       Edit edited -> editedFactors edited



multiplyFactor (Factor a) (Factor b) = Factor (a * b)


type alias ReportModel =
   {
            report: Report
         ,  selection: Maybe Selection
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



tryGetSelection: Model -> Maybe Selection
tryGetSelection  =
   tryGetReportModel 
   >> Maybe.andThen (\model -> model.selection)
   


update: Result String Report -> Maybe Model -> Model
update result model =
   case result of
      Ok report -> ReportWithoutError { report = report , selection = Nothing}
      Err err -> addError model err 


   
