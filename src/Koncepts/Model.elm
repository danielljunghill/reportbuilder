module Koncepts.Model exposing (..)
import Id exposing (..)

-- Koncepts
type ValueKonceptId = ValueKonceptId  Id
type ValueKonceptName = ValueKonceptName String
type alias ValueKoncept =
    {
            name: ValueKonceptName
        ,   id: ValueKonceptId
    }
type AbstractKonceptName = AbstractKonceptName String
type AbstractKonceptId = AbstractKonceptId Id
type alias AbstractKoncept =
    {
            name : AbstractKonceptName
        ,   id : AbstractKonceptId
    }

--Hypercube
type DomainName = DomainName String
type DomainMember = DomainMember String
type alias Domain =
    {
            name: DomainName
        ,   members: List DomainMember 
    }

type DefaultMember = DefaultMember String  
type Dimension =
     DimensionWithDefault  (DefaultMember,Domain)
     | DimensionWithoutDefault Domain

type HyperDimension =
     Opened Dimension
     | Closed  Dimension

type HyperCubeName = HyperCubeName String
type HyperCubeId = HyperCubeId Id
type alias HyperCube =
    {
            name: HyperCubeName
        ,   head: HyperDimension
        ,   tail: List HyperDimension 
        ,   id: HyperCubeId
    }

-- Koncept strukture
type DimensionalKoncept =
    DimensionalAbstract  (AbstractKoncept, List DimensionalKoncept) 
    | DimensionalValue ValueKoncept

type Koncept =
    Cube  (HyperCube, List DimensionalKoncept)
    | Abstract  (AbstractKoncept, List Koncept)
    | Value ValueKoncept


createValueKoncept: String -> ValueKoncept   
createValueKoncept name  =
        {
                id = Id.create() |> ValueKonceptId 
            ,   name = name |> ValueKonceptName }

createAbstractKoncept: String -> AbstractKoncept
createAbstractKoncept name =  
        {
                id = Id.create() |> AbstractKonceptId 
                , name = AbstractKonceptName name                      
        } 


type ModelAction a = 
   Delete 
   | MapValue a
   | Ignore         