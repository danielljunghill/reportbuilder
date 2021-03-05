module Report.Model exposing (..)

import Page.View exposing (..)
import Page.Model as Page
import Koncepts.Model exposing (..)
import Page.Model exposing (..)
import List
import Id exposing (..)

idValue: Id -> String
idValue (Id v) = v


type alias Report = { 
        name: String
    ,   id: Id
    ,   description: String
    ,   pages: List Page
    }


create: Id -> String -> String -> List Page -> Report
create id name description pages  =   
        {
            name = name,
            id = id,
            description = description,
            pages = pages
        }

init: Id -> String -> String -> Report
init id name desc =  create id name desc []

addNewPage: Report -> Report
addNewPage report =
    let
      
        calculatePageName: List Page -> String
        calculatePageName pages =
            pages
            |> List.filter (\p -> String.startsWith "Page" p.name)
            |> List.length
            |> (\nr -> "Page " ++ ((nr + 1) |> String.fromInt))
        name: String
        name = calculatePageName report.pages
        page: Page 
        page = Page.Model.new True (Id name) name name
        
    in

        { report | pages =  (report.pages |> List.map Page.Model.deselect) ++ [ page ] }



addPage: Page -> Report -> Report
addPage page report  =
    { report | pages = report.pages ++ [ page ]}

selectPage: Report -> Page -> Report
selectPage report page =
    let
        select: Page -> Page -> Page
        select page1 page2 =
            case page1.id == page2.id of
            True -> Page.Model.select page2
            False -> Page.Model.deselect page2
    in
        { report | pages = report.pages |> List.map (select page) }
    






