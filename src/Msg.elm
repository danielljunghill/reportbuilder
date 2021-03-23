module Msg exposing (..)
import Koncepts.Model exposing (..)
import Page.Model exposing (Page)
import Koncepts.Model as Koncept
import NList exposing (..)
import Model exposing (..)




-- type Msg =
--    SelectMembers (NList Member)   
--    SelectValue (ValueKoncept)
--    SelectValueAndMembers (ValueKoncept, NList Member)
type Msg =
   Select (NList Factor)
  | Edit  (NList Factor, Content)
  | DoNothing 


-- selectAbstract: Koncept.AbstractKoncept -> Msg
-- selectAbstract =
--    SelectedAbstract >> Select

-- selectValue: Koncept.ValueKoncept -> Msg
-- selectValue =
--    SelectedValue >> Select

-- selectPage: Page -> Msg
-- selectPage =
--    SelectedPage >> Select