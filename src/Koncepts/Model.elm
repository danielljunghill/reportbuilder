module Koncepts.Model exposing (..)
import Id exposing (..)
import Id
import NList exposing (..)
import Prime exposing (..)
import Dict 
import Dict exposing (..)

-- import Events.Custom exposing (onClickStopPropagation)
type Factor = Factor Int
factorToInt (Factor i) = i

type AbstractFactor = AbstractFactor Int

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
        ,   factor: AbstractFactor
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
            

createAbstractKonceptWithSelection: Bool -> String -> AbstractKoncept
createAbstractKonceptWithSelection selected name  =  
        {
                     id = Id.create() |> AbstractKonceptId 
                ,    name = AbstractKonceptName name   
                ,    selected = selected   
                ,    factor = 1 |> AbstractFactor            
        } 

createValueKoncept: String -> Factor ->  ValueKoncept
createValueKoncept = createValueKonceptWithSelection False  

createAbstractKoncept: String -> AbstractKoncept
createAbstractKoncept = createAbstractKonceptWithSelection False

type ModelAction a = 
   Delete 
   | MapValue a
   | Ignore

multiply (Factor a) (Factor b) = Factor (a * b)

multiplyFactors: NList Factor -> Factor
multiplyFactors factors = 
    factors 
    |> NList.fold multiply (Factor 1) 

membersFactorList members =
   members |> NList.map (\m -> m.factor)

membersFactor = membersFactorList >> multiplyFactors

hyperValueFactorList: ValueKoncept -> NList Member  -> NList Factor
hyperValueFactorList vk members =
    members 
    |> membersFactorList
    |> NList.addFirst vk.factor 

hyperValueFactor vk members =
    hyperValueFactorList vk members
    |> multiplyFactors

valueFactor vk =
    vk.factor

type Content = Content String

-- type alias FlatValue =
--     {
--             content: Content
--         ,   koncept: ValueKoncept
--     }

-- type alias HyperValue =
--     {
--             koncept: ValueKoncept
--         ,   members: NList Member
--         ,   content: Content
--     } 

-- type ReportedValue =
--     Hyper HyperValue 
--     | Flat FlatValue


-- reportValueContent: ReportedValue -> Content
-- reportValueContent rv =
--     case rv of
--         Hyper hv -> hv.content
--         Flat fv -> fv.content

type ReportedValueFactor = ReportedValueFactor Factor
type ReportedValue = ReportedValue Content
reportValueContent (ReportedValue content) = content

type alias ReportedValues = 
    {
        values: Dict Int ReportedValue 
    }


emptyReportedValues =
    {
        values = Dict.empty
    }

getReportedValue: ReportedValues -> ReportedValueFactor -> Maybe ReportedValue
getReportedValue reportedValues (ReportedValueFactor (Factor factor))  =
    reportedValues.values
    |> Dict.get factor

insertReportedValue: ReportedValues -> ReportedValue -> ReportedValueFactor  -> ReportedValues
insertReportedValue reportedValues value (ReportedValueFactor (Factor factor)) =
    reportedValues.values  
    |> Dict.insert factor value 
    |> (\dict -> { values = dict})


getContent: ReportedValueFactor -> ReportedValues ->  Maybe Content
getContent factor reportValues =
    factor
    |> getReportedValue reportValues 
    |> Maybe.map reportValueContent

type ValueFetcher = ValueFetcher (ReportedValueFactor -> Maybe ReportedValue)

valueFetcherMock = ValueFetcher (\rvf -> Nothing )
-- createHyper koncept members content = 
    

 


