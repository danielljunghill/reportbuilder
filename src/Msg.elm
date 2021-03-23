module Msg exposing (..)
import Koncepts.Model exposing (..)
import Page.Model exposing (Page)
import Koncepts.Model as Koncept
import NList exposing (..)
import Model exposing (..)



-- type Edited =
--    Value (ValueKoncept,Content)
--    ValueMember (ValueKoncept,Content)

-- type Selected =
--    Value ValueKoncept
--    ValueMember (ValueKoncept,NList Member)
--    Member (NList Member)


-- type Msg =
--    SelectMembers (NList Member)   
--    SelectValue (ValueKoncept)
--    SelectValueAndMembers (ValueKoncept, NList Member)
type Msg =
   SelectMsg Selected
  | EditMsg Edited



-- selectAbstract: Koncept.AbstractKoncept -> Msg
-- selectAbstract =
--    SelectedAbstract >> Select

-- selectValue: Koncept.ValueKoncept -> Msg
-- selectValue =
--    SelectedValue >> Select

-- selectPage: Page -> Msg
-- selectPage =
--    SelectedPage >> Select