module Koncepts.Model exposing (..)
import Id exposing (..)
import Id
import NList exposing (..)
import Prime exposing (..)

-- import Events.Custom exposing (onClickStopPropagation)
type Factor = Factor Int
factorToInt (Factor i) = i

factorFromPrime: Prime -> Factor 
factorFromPrime prime =
    Factor prime.numbers.head


-- Koncepts
type ValueKonceptId = ValueKonceptId  Id
type ValueKonceptName = ValueKonceptName String
-- type Factor = Factor Int

valueKonceptNameToString: ValueKonceptName -> String
valueKonceptNameToString (ValueKonceptName name) = name
type alias ValueKoncept =
    {
            name: ValueKonceptName
        ,   id: ValueKonceptId
        ,   selected: Bool
        ,   factor: Factor
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


type alias Member =
   {
         id : Id
      , name: String
      , factor: Factor
   }

createMember:String ->  Factor -> Member
createMember name factor  =
   {
         id = Id.create()
      ,  factor = factor
      ,  name = name
   }
type DomainMember = DomainMember Member

createDomainMember: String -> Factor ->  DomainMember
createDomainMember name  =
   createMember name  
   >> DomainMember

domainMemberToString: DomainMember -> String
domainMemberToString (DomainMember member) = member.name
type alias Domain =
    {
            name: DomainName
        ,   members: NList DomainMember 
    }

createDomain: DomainName -> NList DomainMember -> Domain
createDomain name m =
    {
            name = name
        ,   members = m
    }

type DefaultMember = DefaultMember Member  
createDefaultMember: String -> Factor -> DefaultMember
createDefaultMember name  =
   createMember name  
   >> DefaultMember

type Dimension =
     DimensionWithDefault  (DefaultMember,Domain)
     | DimensionWithoutDefault Domain


dimensionMembers: Dimension -> NList DomainMember
dimensionMembers dimension =
   case dimension of 
      DimensionWithDefault (_,m) -> m.members
      DimensionWithoutDefault m -> m.members

memberDefault: Dimension -> Maybe DefaultMember
memberDefault dimension =
   case dimension of 
      DimensionWithDefault (d,_) -> Just d
      DimensionWithoutDefault _ -> Nothing
   
type HyperDimension =
     Opened Dimension
     | Closed  Dimension

hyperDimensionAsDimension: HyperDimension -> Dimension
hyperDimensionAsDimension hyperDimension =
    case hyperDimension of
        Opened dimension -> dimension
        Closed dimension -> dimension

type HyperCubeName = HyperCubeName String

hyperCubeNameToString: HyperCubeName -> String
hyperCubeNameToString (HyperCubeName name) = name
type HyperCubeId = HyperCubeId Id
type alias HyperCube =
    {
            name: HyperCubeName
        ,   dimensions: NList HyperDimension
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


createValueKonceptWithSelection: Bool -> String -> Factor   -> ValueKoncept
createValueKonceptWithSelection  selected name factor   =  
        let 
            newValueKoncept: ValueKoncept
            newValueKoncept = 
                {
                        name = name |> ValueKonceptName
                    ,   id = Id.create() |> ValueKonceptId
                    ,   selected = selected
                    ,   factor = factor}
        in 
            newValueKoncept
            

-- createValueKonceptWithSelection: Bool -> String -> ValueKoncept   
-- createValueKonceptWithSelection selected name  =
--         {
--                 id = Id.create() |> ValueKonceptId 
--             ,   name = name |> ValueKonceptName
--             ,   selected = selected }

createAbstractKonceptWithSelection: Bool -> String -> AbstractKoncept
createAbstractKonceptWithSelection selected name  =  
        {
                     id = Id.create() |> AbstractKonceptId 
                ,    name = AbstractKonceptName name   
                ,    selected = selected               
        } 

createValueKoncept: String -> Factor ->  ValueKoncept
createValueKoncept = createValueKonceptWithSelection False  


createAbstractKoncept: String -> AbstractKoncept
createAbstractKoncept = createAbstractKonceptWithSelection False


type ModelAction a = 
   Delete 
   | MapValue a
   | Ignore         



