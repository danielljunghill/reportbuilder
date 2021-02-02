module Koncepts.Mock exposing (..)
import Koncepts.Model exposing (Koncept(..))
import Koncepts.Koncept exposing (..)




head: Result String Koncept
head = 
    "Head abstract1"  
    |> createAbstract 
    |> ParentKoncept  
    |> add ("Sub abstract2" |> createAbstract )

addOne: Result String Koncept
addOne =
    head
    |> Result.map ParentKoncept 
    |> Result.andThen ("First Values" |> createValue |> add)
                         