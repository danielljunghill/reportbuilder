module Msg exposing (..)
import Koncepts.Model exposing (..)
import Page.Model exposing (Page)

type SelectItem =
   Abstract AbstractKoncept
   | Value ValueKoncept
   | Page Page
   
type Msg = 
  Select SelectItem
  | Add 
  | AddPage 