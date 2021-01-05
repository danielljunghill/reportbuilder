module Section.Model exposing (..)
import Koncept.Model as Koncept exposing (..)

type Id = Id String
idValue: Id -> String
idValue (Id v) = v
type alias Section = { 
        name: String,
        id: Id,
        description: String,
        isSelected: Bool,
        koncepts: List Koncept }

create: Id -> String -> String -> List Koncept -> Section
create id name description koncepts =
    {
        name = name,
        id = id,
        description = description,
        koncepts = koncepts,
        isSelected = False
    }




