module Report.Mock exposing (..)
import Page.Model as Page
import Page.Model exposing (..)
import Koncepts.Mock as Koncept
import Report.Model as Report
import Report.Model exposing (..)
import Id 

page1: Result String Page
page1 = 
   Koncept.mockKoncept
   |> Result.map (\koncept -> Page.create [ koncept ] True (Id.create()) "IORP2" "IORP2 nation")


report: Page ->  Result String Report
report page =
   Report.create (Id.create()) "IORP2" "IORP2" [ page ] 
   |> Ok

mockReport: Result String Report
mockReport = page1 |> Result.andThen report 


