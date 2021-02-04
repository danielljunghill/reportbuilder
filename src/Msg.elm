module Msg exposing (..)
import Koncepts.Model exposing (..)
import Page.Model exposing (Page)
import Koncepts.Model as Koncept

type SelectItem =
   SelectAbstract AbstractKoncept
   | SelectValue ValueKoncept
   | SelectPage Page
   
type Msg =
   Select SelectItem
  | Add 
  | AddPage 

selectAbstract: Koncept.AbstractKoncept -> Msg
selectAbstract =
   SelectAbstract >> Select

selectValue: Koncept.ValueKoncept -> Msg
selectValue =
   SelectValue >> Select

selectPage: Page -> Msg
selectPage =
   SelectPage >> Select