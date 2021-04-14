module Koncepts.CubeRowHeader exposing (createIndentedHeader)
import Koncepts.Model exposing (Member, ValueKoncept, AbstractKoncept,abstractKonceptNameToString, valueKonceptNameToString,DimensionalKoncept(..))
import Koncepts.Area exposing (..)
import Koncepts.CubeModel exposing(CubeHeader,Indent(..),incrementIndent)
import Model exposing (Selection(..),abstractFactorsInSelection,factorsInSelection)
import Lists exposing(contains)
import NList exposing(..)



fromAbstract:Bool -> Indent -> AbstractKoncept -> CubeHeader 
fromAbstract isSelected indent abstractKoncept =
    {
            column = Start 1
        ,   columnSpan = Span 1
        ,   row = Start 1
        ,   attributes = [ "abstract-header" ]
        ,   rowSpan = Span 1
        ,   name = abstractKonceptNameToString abstractKoncept.name
        ,   isSelected = isSelected
        ,   indent = Just indent
    }

fromValue: Bool -> Indent -> ValueKoncept -> CubeHeader 
fromValue isSelected indent valueKoncept =
    {
            column = Start 1
        ,   columnSpan = Span 1
        ,   row = Start 1
        ,   attributes = [ "value-header" ]
        ,   rowSpan = Span 1
        ,   name = valueKonceptNameToString valueKoncept.name
        ,   isSelected = isSelected
        ,   indent = Just indent
    }

selectionContainsAbstract: Maybe Selection -> AbstractKoncept -> Bool
selectionContainsAbstract maybeSelection ak =
    let 
        isAbstractInSelection selection  =
            selection
            |> abstractFactorsInSelection
            |> Lists.contains ak.factor
    in
       maybeSelection
       |> Maybe.map isAbstractInSelection
       |> Maybe.withDefault False 

selectionContainsValue: Maybe Selection -> ValueKoncept -> Bool
selectionContainsValue maybeSelection vk =
    let 
        isValueInSelection selection  =
            selection
            |> factorsInSelection
            |> Lists.contains vk.factor
    in
       maybeSelection
       |> Maybe.map isValueInSelection
       |> Maybe.withDefault False 


createIndentedHeader: Maybe Selection -> List DimensionalKoncept -> List CubeHeader
createIndentedHeader maybeSelection dimensionalKoncepts =
    let 
        recFirst indent koncept =
            case koncept of
                DimensionalAbstract (ak, koncepts) -> 
                    let 
                        isSelected = selectionContainsAbstract maybeSelection ak
                        firstHeader = fromAbstract isSelected indent ak
                    in 
                        [ firstHeader ] ++ (koncepts |> Lists.collect (recRest (incrementIndent indent) firstHeader))
                DimensionalValue vk -> 
                    let
                        isSelected = selectionContainsValue maybeSelection vk
                    in
                        [ fromValue isSelected indent vk ]

        recRest indent parent koncept =
            case koncept of
                DimensionalAbstract (ak, koncepts) -> 
                    let 
                        isSelected = parent.isSelected && (selectionContainsAbstract maybeSelection ak)
                        firstHeader = fromAbstract isSelected indent ak
                    in 
                        [ firstHeader ] ++ (koncepts |> Lists.collect (recRest (incrementIndent indent) firstHeader))
                DimensionalValue vk -> 
                    let
                        isSelected = parent.isSelected && selectionContainsValue maybeSelection vk
                    in
                        [ fromValue isSelected indent vk ]
    in
              dimensionalKoncepts |> Lists.collect (recFirst (Indent 1))
              |> Lists.mapi (\index header -> { header | row = Start (index + 1)})




     