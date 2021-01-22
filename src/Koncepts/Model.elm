module Koncepts.Model exposing (..)
import Id exposing (..)
type ValueKonceptId = ValueKonceptId  Id
type ValueKonceptName = ValueKonceptName String
type alias ValueKoncept =
    {
            name: ValueKonceptName
        ,   id: ValueKonceptId
    }

type DomainName = DomainName String
type DomainMember = DomainMember String
type alias Domain =
    {
            name: DomainName
        ,   members: List DomainMember 
    }


domainCreate: String -> List String -> Domain
domainCreate name members =
    {
            name = DomainName name
        ,   members = members |> List.map DomainMember
    }

type DefaultMember = DefaultMember String  
type Dimension =
     DimensionWithDefault  (DefaultMember,Domain)
     | DimensionWithoutDefault Domain

createDimensionWithDefault: Domain -> Dimension
createDimensionWithDefault domain =
    DimensionWithDefault (DefaultMember "Total", domain)

createDimensionWithoutDefault: Domain -> Dimension
createDimensionWithoutDefault domain =
    DimensionWithoutDefault domain

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

type DimensionalAbstractKonceptId = DimensionalAbstractKonceptId  Id
type DimensionalAbstractKonceptName = DimensionalAbstractKonceptName String

type alias DimensionalAbstractKoncept =
    {
            id : DimensionalAbstractKonceptId
        ,   name: DimensionalAbstractKonceptName
    }

type AbstractKonceptName = AbstractKonceptName String
type AbstractKonceptId = AbstractKonceptId Id
type alias AbstractKoncept =
    {
            name : AbstractKonceptName
        ,   id : AbstractKonceptId
    }

type DimensionalKoncept =
    DimensionaAbstract  (AbstractKoncept, List DimensionalKoncept) 
    | DimensionaValue ValueKoncept

type Koncept =
    Cube  (HyperCube, List DimensionalKoncept)
    | Abstract  (AbstractKoncept, List Koncept)
    | Value ValueKoncept