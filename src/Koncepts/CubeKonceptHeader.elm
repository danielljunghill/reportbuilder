module Koncepts.CubeKonceptHeader exposing (KonceptHeaderKoncept,KonceptHeader,addMembers,createForIndentedTable)
import Koncepts.Model exposing (Member, ValueKoncept, AbstractKoncept)
import Koncepts.Area exposing (..)

-- type KonceptHeaderKoncept = 
--    Value ValueKoncept
--    | Abstract AbstractKoncept

-- type KonceptHeader =
--    {
--          koncept: KonceptHeaderKoncept
--       ,  members: List Member
--    }

-- fromAbstract ak = 
--       {
--             koncept =  Abstract ak
--          ,  members = []
--       }
     
-- fromValue vk = 
--       {
--             koncept =  Value vk
--          ,  members = []
--       }

-- addMembers members m =§
--       { m | members = (members |> NList.toList)  @ m.Members}

-- createForIndentedTable: List DimensionalKoncept -> List TableHeader KonceptHeader
-- createForIndentedTable koncepts =
--     let 
--         recCreate depth start koncept =
--             case koncept of
--                 DimensionalAbstract (ak, koncepts) -> 
--                     [ 
--                         { Start = start ; Span = Span 1 ; Depth = depth; Item = fromAbstract ak }
--                     ] @ (koncepts |> List.collect (recCreate (incrementDepth depth) (incrementStart start)))
--                 DimensionalValue vk ->  [{ Start = start ; Span = Span 1 ; Depth = depth; Item = fromValue vk }]

--             koncepts |> List.collect (recCreate (Depth 1) (Start 1))
--     in
--         recCreate

createCubeHeaderFromAbstract:Bool -> Start -> Depth -> Span -> AbstractKoncept -> CubeHeader 
createCubeHeaderFromAbstract isSelected start depth span abstractKoncept =
    {
            span = span
        ,   start = Start
        ,   depth = Depth
        ,   attributes = ["abstract-header"]
        ,   name = abstractKonceptNameToString abstractKoncept
        ,   isSelected = isSelected
    }

createCubeHeaderFromValue: Bool -> Start -> Depth -> Span -> AbstractKoncept -> CubeHeader 
createCubeHeaderFromValue isSelected start depth span abstractKoncept =
    {
            span = span
        ,   start = Start
        ,   depth = Depth
        ,   attributes = []
        ,   name = abstractKonceptNameToString abstractKoncept
        ,   isSelected = False
    }

selectionContainsAbstractKoncept: Maybe Selection -> AbstractKoncept -> Bool
selectionContainsAbstractKoncept maybeSelection ak =
    let 
        isAbstractInSelection selection  =
            selection
            |> abstractFactorsInSelection
            |> List.contains abstractKoncept.factor
    in
       maybeSelection
       |> Maybe.map isAbstractInSelection
       |> Maybe.withDefault False 

selectionContainsValueKoncept: Maybe Selection -> ValueKoncept -> Bool
selectionContainsValueKoncept maybeSelection vk =
    let 
        isValueInSelection selection  =
            selection
            |> factorsInSelection
            |> List.contains vk.factor
    in
       maybeSelection
       |> Maybe.map isValueInSelection
       |> Maybe.withDefault False 

indentedColSpan: DimensionalKoncept -> Int
indentedColSpan dimKoncept =
   let 
      recCalcForOne: Int -> DimensionalKoncept -> List Int
      recCalcForOne stateOne koncept =
         let
            newStateOne: Int
            newStateOne = 1
         in
            case koncept of
               DimensionalAbstract (_,childKoncepts) ->
                  let
                     recCalcForMany: Int -> List DimensionalKoncept -> List Int
                     recCalcForMany stateMany koncepts =
                        case koncepts of
                           [] -> [ stateMany ]
                           head :: tail ->  (recCalcForOne stateMany head) ++ recCalcForMany stateMany tail
                  in
                     recCalcForMany newStateOne childKoncepts
               DimensionalValue _ -> [ newStateOne ]
     
   in
      recCalcForOne 0 dimKoncept
      |> Lists.maxInt
    



createIndentedHeaderForKoncept: Maybe Selection -> List DimensionalKoncept -> List CubeHeader
createIndentedHeaderForKoncept maybeSelection koncepts =
    let 
        maxColSpan = indentedColSpan koncepts
    in
        let 
            recFirst depth start koncept =
                case koncept of
                    DimensionalAbstract (ak, koncepts) -> 
                        let 
                            isSelected = selectionContainsAbstractKoncept maybeSelection ak
                            firstHeader = createCubeHeaderFromAbstract isSelected start depth span ak
                        in 
                            [ firstHeader ] @ (koncepts |> List.collect (recRest (incrementDepth depth) (incrementStart start) firstHeader))
                    DimensionalValue vk -> 
                        isSelected = selectionContainsValueKoncept maybeSelection vk
                        createCubeHeaderFromValue isSelected start (Span 1) depth vk

            recRest depth start parent koncept =
                case koncept of
                    DimensionalAbstract (ak, koncepts) -> 
                        let 
                            isSelected = parent.isSelected && selectionContainsAbstractKoncept maybeSelection ak
                            firstHeader = createCubeHeaderFromAbstract isSelected start depth span ak
                        in 
                            [ firstHeader ] @ (koncepts |> List.collect (recRest (incrementDepth depth) (incrementStart start) firstHeader))
                    DimensionalValue vk -> 
                        isSelected = parent.isSelected && selectionContainsValueKoncept maybeSelection vk
                        createCubeHeaderFromValue isSelected start (Span 1) depth vk
        in
              koncepts |> List.collect (recFirst (Depth (maxColSpan + 1)) (Start 1))




-- createIndentedHeaderForKoncept: Maybe Selection -> List DimensionalKoncept -> List CubeHeader
-- createIndentedHeaderForKoncept maybeSelection koncepts =
--     let 
--         maxColSpan = indentedColSpan koncepts
--     in
--         let 
--             recCreate depth start koncept =
--                 case koncept of
--                     DimensionalAbstract (ak, koncepts) -> 
--                         [ 
--                             createCubeHeaderFromAbstract start depth span abstractKoncept =
--                         ] @ (koncepts |> List.collect (recCreate (incrementDepth depth) (incrementStart start)))
--                     DimensionalValue vk ->  [{ Start = start ; Span = Span 1 ; Depth = depth; Item = fromValue vk }]

--                 koncepts |> List.collect (recCreate (Depth 1) (Start 1))
--         in
--             recCreate

     