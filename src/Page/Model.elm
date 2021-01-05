module Page.Model exposing (..)
import Section.Model as Section exposing (..)
import Html.Attributes exposing (selected)


type Id = Id String
idValue: Id -> String
idValue (Id v) = v

type alias Page = {
    name: String,
    id: Id,
    description: String,
    sections: List Section,
    selected: Bool
    }

create:  List Section -> Bool -> Id -> String -> String ->  Page
create sections selected  id name description    =
    {
        name = name,
        id = id,
        description = description,
        sections = sections,
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

    
addSection: Section -> Page -> Page
addSection section page  =
  { page | sections = [ section ] |> List.append page.sections }  






