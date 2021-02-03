module Page.Model exposing (..)
import Koncepts.Model as Koncepts exposing (..)
import Html.Attributes exposing (selected)


type Id = Id String
idValue: Id -> String
idValue (Id v) = v

type alias Page = {
    name: String,
    id: Id,
    description: String,
    koncepts: List Koncept,
    selected: Bool
    }

create:  List Koncept -> Bool -> Id -> String -> String ->  Page
create koncepts selected  id name description    =
    {
        name = name,
        id = id,
        description = description,
        koncepts = koncepts,
        selected = selected
    }

deselect: Page -> Page
deselect page =
    { page | selected = False }

select: Page -> Page
select page = 
    { page | selected = True }


new: Bool -> Id -> String -> String ->  Page 
new = create [] 

init: Id -> String -> String -> Page
init = new True

    
addKoncept: Koncept -> Page -> Page
addKoncept koncept page  =
  { page | koncepts = [ koncept ] |> List.append page.koncepts }  

addKoncepts: List Koncept -> Page -> Page
addKoncepts koncepts page  =
  { page | koncepts =  koncepts |> List.append page.koncepts }  








