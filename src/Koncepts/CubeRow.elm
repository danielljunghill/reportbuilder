module Koncepts.CubeRow exposing (..)
import Koncepts.Model exposing (Member, ValueKoncept, AbstractKoncept, DimensionalKoncept(..),AbstractFactor, Factor, multiplyFactors)
import Koncepts.Area exposing (..)
import NList exposing (..)
import Lists 


type alias CubeRowContext =     
    {
            abstracts: List AbstractKoncept 
        ,   members: List Member 
    }

emptyCubeRowContext =
    {
            abstracts = []
        ,   members = [] 
    }

cubeRowContextAddMembers:  List Member -> CubeRowContext -> CubeRowContext
cubeRowContextAddMembers members context   =
    { context | members = context.members ++ members }

cubeRowContextAddAbstractKoncept:  AbstractKoncept -> CubeRowContext -> CubeRowContext
cubeRowContextAddAbstractKoncept  ak context =
    { context | abstracts = context.abstracts ++ [ ak ] }

type CubeValueRow = CubeValueRow (ValueKoncept,CubeRowContext)

createCubeValueRow vk = (vk, emptyCubeRowContext) |> CubeValueRow

type CubeAbstractRow = CubeAbstractRow (AbstractKoncept,CubeRowContext)

createCubeAbstractRow ak =  (ak, emptyCubeRowContext) |> CubeAbstractRow 
cubeAbstractRowAddAbstractKoncept (CubeAbstractRow (ak,context)) newAk =
    let 
        newContext = 
            context 
            |> cubeRowContextAddAbstractKoncept ak
    in
        CubeAbstractRow (newAk,newContext)

cubeAbstractRowAddValueKoncept (CubeAbstractRow (ak,context)) vk =
    let 
        newContext = 
            context 
            |> cubeRowContextAddAbstractKoncept ak
    in
        CubeValueRow (vk,newContext)

maybeCubeAbstractRowAddAbstractKoncept: AbstractKoncept -> Maybe CubeAbstractRow -> CubeAbstractRow
maybeCubeAbstractRowAddAbstractKoncept koncept state =
    case state of
        Just row ->
            koncept  
            |> cubeAbstractRowAddAbstractKoncept row
        Nothing ->
            createCubeAbstractRow koncept

maybeCubeAbstractRowAddValueKoncept: ValueKoncept -> Maybe CubeAbstractRow -> CubeValueRow
maybeCubeAbstractRowAddValueKoncept koncept state =
    case state of
        Just row ->
            koncept
            |> cubeAbstractRowAddValueKoncept row  
        Nothing ->
            createCubeValueRow koncept
    

type CubeRow =
    AbstractRow CubeAbstractRow 
    | ValueRow CubeValueRow

cubeValueRowAbstractFactors: CubeValueRow -> List AbstractFactor
cubeValueRowAbstractFactors (CubeValueRow (_,context)) =
    context.abstracts |> List.map (\ak -> ak.factor)

cubeAbstractRowAbstractFactors: CubeAbstractRow -> List AbstractFactor
cubeAbstractRowAbstractFactors (CubeAbstractRow (_,context)) =
    context.abstracts |> List.map (\ak -> ak.factor)

cubeValueRowFactors: CubeValueRow -> NList Factor 
cubeValueRowFactors (CubeValueRow (vk,context))  =
    vk.factor
    |> NList.create 
    |> NList.addList (context.members |> List.map (\m -> m.factor))

cubeValueRowFactor cubeValueRow =
    cubeValueRow
    |> cubeValueRowFactors
    |> NList.toList
    |> multiplyFactors
           
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