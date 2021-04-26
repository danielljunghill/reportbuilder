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