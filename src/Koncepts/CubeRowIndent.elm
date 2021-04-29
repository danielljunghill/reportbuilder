module Koncepts.CubeRowIndent exposing (..)
import Koncepts.CubeRow exposing (..)
import Koncepts.Model exposing (Member, ValueKoncept, AbstractKoncept, DimensionalKoncept(..),AbstractFactor, Factor, multiplyFactors)
import Koncepts.Area exposing (..)
import NList exposing (..)
import Lists 


           
addMembers: NList Member  -> CubeRow -> CubeRow 
addMembers members cubeRow =
    case cubeRow of
        AbstractRow (CubeAbstractRow (ak, context)) ->  
            (ak, context  |> cubeRowContextAddMembers (members |> NList.toList))
            |> CubeAbstractRow 
            |> AbstractRow
        ValueRow (CubeValueRow (ak, context)) -> 
            (ak, context |> cubeRowContextAddMembers (members |> NList.toList) ) 
            |> CubeValueRow 
            |> ValueRow

addAbstractKoncept: AbstractKoncept -> CubeRow ->  CubeRow
addAbstractKoncept newAk cubeRow   =
     case cubeRow of
        AbstractRow (CubeAbstractRow (ak, context)) ->  
            (ak, context |> cubeRowContextAddAbstractKoncept newAk )
            |> CubeAbstractRow 
            |> AbstractRow
        ValueRow (CubeValueRow (ak, context)) -> 
            (ak,  context|> cubeRowContextAddAbstractKoncept newAk ) 
            |> CubeValueRow 
            |> ValueRow

createIndented: List DimensionalKoncept -> List CubeRow
createIndented dimensionalKoncepts = 
    let 
        recfirst: DimensionalKoncept -> List CubeRow
        recfirst koncept =
            case koncept of
                DimensionalAbstract (ak, koncepts) -> 
                    [ 
                        ak |> createCubeAbstractRow |> AbstractRow
                    ] ++ (koncepts |> Lists.collect (recRest ak))
                DimensionalValue vk -> [ vk |> createCubeValueRow |> ValueRow  ]

        recRest: AbstractKoncept -> DimensionalKoncept -> List CubeRow
        recRest parent koncept =
            case koncept of
                DimensionalAbstract (ak, koncepts) -> 
                    [ 
                        ak
                        |> createCubeAbstractRow 
                        |> AbstractRow 
                        |> addAbstractKoncept parent
                    ] ++ (koncepts |> Lists.collect (recRest ak))
                DimensionalValue vk ->
                    [ 
                        createCubeValueRow vk
                        |> ValueRow
                        |> addAbstractKoncept parent
                    ]
    in 
        dimensionalKoncepts |> Lists.collect recfirst




cubeRowsForTree: Maybe CubeAbstractRow -> List DimensionalKoncept -> List CubeRow
cubeRowsForTree parent dimensionalKontexts =
    case dimensionalKontexts of
        [] -> []
        head :: tail ->
            let 
                recKontextHeader state kontext =
                    case kontext of
                        DimensionalAbstract (ak, koncepts) ->
                            let 
                                abstractRow = state |> maybeCubeAbstractRowAddAbstractKoncept ak
                            in
                                case koncepts of
                                    [] -> [ abstractRow |> AbstractRow ]
                                    child :: restChildren ->  
                                        (recKontextHeader (Just abstractRow) child) ++ (cubeRowsForTree state restChildren)
                        DimensionalValue vk ->
                                [ 
                                    state 
                                    |> maybeCubeAbstractRowAddValueKoncept vk
                                    |> ValueRow   
                                ]
            in
                (recKontextHeader parent head) ++ (cubeRowsForTree parent tail)