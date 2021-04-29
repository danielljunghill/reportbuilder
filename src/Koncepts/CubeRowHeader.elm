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



-- Generate tree for koncepts TREE
-- 


rowSpanTree: DimensionalKoncept -> RowSpan
rowSpanTree  =
    let 
        recRowSpan koncept =
            case koncept of
                DimensionalAbstract (_, childKoncepts) ->
                    case childKoncepts of
                        [] -> 1
                        _ ->
                            let 
                                recSpan koncepts =
                                    case koncepts of
                                        [] -> 0
                                        head :: tail ->
                                            (recRowSpan head) + (recSpan tail)
                            in    
                                recSpan childKoncepts
                DimensionalValue _ -> 1
    in
        recRowSpan >> RowSpan

colSpanTree  =
    let 
        recCalcForOne stateOne koncept =      
            let 
                newStateOne = stateOne + 1
            in 
                case koncept of
                    DimensionalAbstract (_,childKoncepts) ->
                        let 
                            recCalcForMany stateMany koncepts =
                                case koncepts of
                                    [] -> [ stateMany ]
                                    head :: tail ->  (recCalcForOne stateMany head) ++ recCalcForMany stateMany tail     
                        in           
                            recCalcForMany newStateOne childKoncepts
                    DimensionalValue _ -> [ newStateOne ] 
    in
        
        recCalcForOne 0
        >> Lists.maxInt
        >> Maybe.withDefault 1
        >> ColumnSpan

cubeRowHeadersTree: List DimensionalKoncept -> List CubeRowHeader
cubeRowHeadersTree dimensionalKontexts =
    let 
        maxColSpanForTree =
            dimensionalKontexts
            |> List.map colSpanTree
            |> List.map intColumnSpan
            |> Lists.maxInt
            |> Maybe.withDefault 1
            |> ColumnSpan
   
        recCubeHeaders: ColumnSpan -> Area -> List DimensionalKoncept -> List CubeHeader
        recCubeHeaders maxColSpan arean kontexts  = 
            case kontexts of
                [] -> []
                head :: tail ->
                    let 
                        rowSpan = rowSpanTree head
                        recCubeHeadersInner: ColumnSpan -> Area -> DimensionalKoncept -> List CubeHeader
                        recCubeHeadersInner maxColSpanForKoncept area childKoncepts =
                            let 
                                newMaxColSpan = decColSpan maxColSpanForKoncept 
                            in
                                case childKoncepts of
                                    DimensionalAbstract (ak, koncepts) ->
                                        case koncepts of
                                            [] -> [{ 
                                                            area = { area | columnSpan = maxColSpanForKoncept }
                                                        ,   name = abstractKonceptNameToString ak.name 
                                                        ,   indent = Nothing
                                                        ,   isSelected = False
                                                        ,   attributes = []
                                                    }]
                                            child :: children ->                       
                                                [{ 
                                                            area =    { area | rowSpan = rowSpan }
                                                        ,   name = abstractKonceptNameToString ak.name 
                                                        ,   indent = Nothing
                                                        ,   isSelected = False
                                                        ,   attributes = []
                                                    }]
                                                |> List.append (recCubeHeadersInner newMaxColSpan ({ area | column = incColumn area.column }) child)
                                                |> List.append (recCubeHeaders newMaxColSpan ({ area | column = incColumn area.column , row = incRow area.row }) children)

                                    DimensionalValue vk ->
                                        [{ 
                                                area =  { area | columnSpan = maxColSpanForKoncept }
                                            ,   name = valueKonceptNameToString vk.name 
                                            ,   indent = Nothing
                                            ,   isSelected = False
                                            ,   attributes = []
                                        }]
                    in
                        recCubeHeadersInner maxColSpan arean head 
                        |> List.append (recCubeHeaders maxColSpan ({arean | row = arean.row |> addRowSpanToRow rowSpan}) tail)
    in
        dimensionalKontexts
        |> recCubeHeaders maxColSpanForTree oneArea      
        |> List.map CubeRowHeader
