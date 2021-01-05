module Msg exposing (..)
import Koncept.Model as Koncept exposing (..)
import Page.Model exposing (Page)


type Msg = 
  Select KonceptInformation
  | SelectPage Page
  | Add
  | AddPage