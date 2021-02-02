module Report.Mock exposing (..)
import Koncept.Model exposing (..)
import Page.Model as Pages exposing (..)
import Report.Model as Reports exposing (..)

k1: Koncept
k1 =  { name = "Intäkter", selected = False, id = "Z1"} |> ValueKoncept |> Value

k2: Koncept
k2 = { name = "Utgifter", selected = False, id = "Z2"}  |> ValueKoncept |> Value

k3:Koncept
k3 = add (ParentKoncept k1) ({ name = "Intäkter per år", selected = False, id = "Z12"}  |> ValueKoncept |> Value)

k4:Koncept
k4 = add (ParentKoncept k2) ({ name = "Utgifter per år", selected = False, id = "Z22"}  |> ValueKoncept |> Value)



page1: Pages.Page
page1 = 
    Pages.new True (Pages.Id "Page1") "Page1" "Page1"
    |> addKoncepts [ k1, k3]

page2: Pages.Page
page2 = 
    Pages.new False (Pages.Id "Page2") "Page2" "Page2"
    |> addKoncepts [ k2, k4]

report: Reports.Report
report  =
     Reports.init (Reports.Id "Report1") "Report1" "Report1"
     |> addPage page1
     |> addPage page2





