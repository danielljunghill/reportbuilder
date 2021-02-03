module Koncepts.Model exposing (..)
import Id exposing (..)


-- Koncepts
type ValueKonceptId = ValueKonceptId  Id
type ValueKonceptName = ValueKonceptName String

valueKonceptNameToString: ValueKonceptName -> String
valueKonceptNameToString (ValueKonceptName name) =
   name
type alias ValueKoncept =
    {
            name: ValueKonceptName
        ,   id: ValueKonceptId
        ,   selected: Bool
    }
type AbstractKonceptName = AbstractKonceptName String

abstractKonceptNameToString: AbstractKonceptName -> String
abstractKonceptNameToString (AbstractKonceptName name) =
   name
type AbstractKonceptId = AbstractKonceptId Id
type alias AbstractKoncept =
    {
            name : AbstractKonceptName
        ,   id : AbstractKonceptId
        ,   selected: Bool
    }

--Hypercube
type DomainName = DomainName String
domainNameToString: DomainName -> String
domainNameToString (DomainName name) = name
type DomainMember = DomainMember String

domainMemberToString: DomainMember -> String
domainMemberToString (DomainMember name) = name
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

hyperCubeNameToString: HyperCubeName -> String
hyperCubeNameToString (HyperCubeName name) = name
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

createValueKonceptWithSelection: Bool -> String -> ValueKoncept   
createValueKonceptWithSelection selected name  =
        {
                id = Id.create() |> ValueKonceptId 
            ,   name = name |> ValueKonceptName
            ,   selected = selected }

createAbstractKonceptWithSelection: Bool -> String -> AbstractKoncept
createAbstractKonceptWithSelection selected name  =  
        {
                     id = Id.create() |> AbstractKonceptId 
                ,    name = AbstractKonceptName name   
                ,    selected = selected               
        } 

createValueKoncept: String -> ValueKoncept   
createValueKoncept = createValueKonceptWithSelection False


createAbstractKoncept: String -> AbstractKoncept
createAbstractKoncept = createAbstractKonceptWithSelection False


type ModelAction a = 
   Delete 
   | MapValue a
   | Ignore         