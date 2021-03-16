module Koncepts.Hypercube exposing (..)
import Id exposing (..)
import ResultExtension exposing (..)
import Koncepts.Model exposing (..)
import NList exposing(..)
import NList as NList
import Prime exposing (..)
import Tuple exposing (..)
---

---
createMembersWithPrime:  NList String  -> Prime ->  PrimeResult (NList DomainMember )
createMembersWithPrime memberNames firstPrime   =
    let
        addNewMember: PrimeResult DomainMember -> PrimeResult (List DomainMember) -> PrimeResult (List DomainMember)
        addNewMember result m  = 
            m 
            |> mapPrimeResult (\r ->  [ result.result ] ++ r )
        recCreateMembers: List String  -> Prime -> PrimeResult (List DomainMember)
        recCreateMembers members prime  =
            case members of
                [] -> 
                    -- let
                    --     emptyMembers: List Member
                    --     emptyMembers = []
                    -- in
                    createPrimeResult [] prime
                head :: tail ->
                    let
                        newMember: DomainMember
                        newMember =
                             prime
                             |> factorFromPrime
                             |> createMember head 
                             |> DomainMember

                        newResult: PrimeResult DomainMember
                        newResult =
                            prime
                            |> generatePrime
                            |> createPrimeResult newMember
      

                    in
                      addNewMember newResult (recCreateMembers tail newResult.prime)
    in
        let 
            firstMember: DomainMember
            firstMember = 
                  firstPrime
                  |> Debug.log "firstPrime"
                  |> factorFromPrime
                  |> (createMember memberNames.head) 
                  |> DomainMember

            tailMembers: PrimeResult (List DomainMember)
            tailMembers = 
                firstPrime
                |> generatePrime
                |> recCreateMembers memberNames.tail 
                |> Debug.log "tailMembers"
        in
            tailMembers
            |> Debug.log "tailMembers"
            |> mapPrimeResult (\m -> NList.create2 firstMember m)
         
            

createDefaultMemberWithPrime: String -> Prime -> PrimeResult DefaultMember
createDefaultMemberWithPrime name prime  =
    {
            prime = generatePrime prime 
        ,   result =  
                    prime 
                    |> factorFromPrime 
                    |> createDefaultMember "Total" 
        }

-- createHyperDimensionWithDefault: String -> NList 

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

createDimensionWithDefault: DomainName -> NList String -> Prime -> PrimeResult Dimension
createDimensionWithDefault name members prime  =
    let
        defaultMember: PrimeResult DefaultMember 
        defaultMember =  
            prime 
            |> createDefaultMemberWithPrime "Total"  
        domain: PrimeResult Domain
        domain = 
            defaultMember.prime
            |> createMembersWithPrime members
            |> mapPrimeResult (createDomain name)
    in 
        domain
        |> mapPrimeResult (\d -> DimensionWithDefault (defaultMember.result, d))

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

addDimension: HyperDimension -> HyperCube -> HyperCube
addDimension dimension hyperCube  =
   { hyperCube | dimensions = NList.append hyperCube.dimensions [ dimension ]}
