module Koncepts.Hypercube exposing (..)
import Id exposing (..)
import ResultExtension exposing (..)
import Koncepts.Model exposing (..)
import NList exposing(..)
import NList as NList
import Prime exposing (..)

---

---
createMembers: Prime -> List String -> (Prime, List DomainMember)
createMembers firstPrime m  =
    let
        addNewMember: DomainMember -> (Prime, List DomainMember) -> (Prime, List DomainMember)
        addNewMember m1 (p,m2) = (p, [ m1 ] ++ m2)
        recCreateMembers: Prime -> List String  -> (Prime,List DomainMember)
        recCreateMembers prime members =
            case members of
                [] -> 
                    -- let
                    --     emptyMembers: List Member
                    --     emptyMembers = []
                    -- in
                    (prime,[])
                head :: tail ->
                    let
                        newMember: DomainMember
                        newMember =
                             head
                             |> createMember prime 
                             |> DomainMember
                        newPrime: Prime
                        newPrime = generatePrime prime                        
                    in
                      addNewMember newMember (recCreateMembers newPrime tail)
    in
        m
        |> recCreateMembers firstPrime 



domainCreate: String -> NList DomainMember -> Domain
domainCreate name members =
    {
            name = DomainName name
        ,   members = members
            --    members 
            --    |> NList.map (createDomainMember (Factor 1))           
    }

domainAddMember : DomainMember -> Domain ->  Domain
domainAddMember member domain  =
      {  domain | members =  NList.append domain.members [ member ] }

createDimensionWithDefault: Prime -> Domain -> Dimension
createDimensionWithDefault prime domain =
    DimensionWithDefault (createDefaultMember prime "Total", domain)

createDimensionWithoutDefault: Domain -> Dimension
createDimensionWithoutDefault domain =
    DimensionWithoutDefault domain

create: String -> HyperDimension -> HyperCube
create name dimension =
        {
                        id = Id.create() |> HyperCubeId
                ,       name = HyperCubeName name
                ,       dimensions = NList.create dimension

        }

addDimension: HyperCube -> HyperDimension -> HyperCube
addDimension hyperCube dimension =
   { hyperCube | dimensions = NList.append hyperCube.dimensions [ dimension ]}
