module Koncepts.CubeRowHeader exposing (createIndentedHeader,CubeRowHeader(..))
import Koncepts.Model exposing (Member, ValueKoncept, AbstractKoncept,abstractKonceptNameToString, valueKonceptNameToString,DimensionalKoncept(..))
import Koncepts.Area exposing (..)
import Koncepts.CubeModel exposing(CubeHeader,Indent(..),incrementIndent)
import Model exposing (Selection(..),abstractFactorsInSelection,factorsInSelection)
import Lists exposing(contains)
import NList exposing(..)

type CubeRowHeader = CubeRowHeader CubeHeader

fromAbstract:Bool -> Indent -> AbstractKoncept -> CubeHeader 
fromAbstract isSelected indent abstractKoncept =
    {
            area = oneArea
        ,   attributes = [ "abstract-header" ]
        ,   name = abstractKonceptNameToString abstractKoncept.name
        ,   isSelected = isSelected
        ,   indent = Just indent
    }

fromValue: Bool -> Indent -> ValueKoncept -> CubeHeader
fromValue isSelected indent valueKoncept =
    {
            area = oneArea
        ,   attributes = [ "value-header" ]
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


createIndentedHeader: Maybe Selection -> List DimensionalKoncept -> List CubeRowHeader
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
        let
            createCubeRowHeder: Int -> CubeHeader -> CubeRowHeader
            createCubeRowHeder index header =
                let 
                    area = header.area
                    newArea = {  area | row = Row index }
                in
                    { header | area  = newArea } |> CubeRowHeader

        in
              dimensionalKoncepts |> Lists.collect (recFirst (Indent 1))
              |> Lists.mapi (\index header -> header |> createCubeRowHeder index )




     