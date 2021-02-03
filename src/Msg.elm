module Msg exposing (..)
import Koncepts.Model as Koncept exposing (..)
import Page.Model exposing (Page)


type Msg = 
  Select Koncept.Koncept
  | SelectPage Page
  | Add
  | AddPage