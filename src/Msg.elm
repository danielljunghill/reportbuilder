module Msg exposing (..)
import Koncepts.Model exposing (..)
import Page.Model exposing (Page)
import Koncepts.Model as Koncept
import NList exposing (..)
import Model exposing (..)


type Content = Content String
   
type Msg =
   Select (NList Factor)
  | Edit  (NList Factor, Content)


-- selectAbstract: Koncept.AbstractKoncept -> Msg
-- selectAbstract =
--    SelectedAbstract >> Select

-- selectValue: Koncept.ValueKoncept -> Msg
-- selectValue =
--    SelectedValue >> Select

-- selectPage: Page -> Msg
-- selectPage =
--    SelectedPage >> Select