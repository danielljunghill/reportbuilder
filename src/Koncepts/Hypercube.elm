module Koncepts.Hypercube exposing (..)
import Id exposing (..)
import ResultExtension exposing (..)
import Koncepts.Model exposing (..)
import NList exposing(..)
import NList as NList

domainCreate: String -> NList String -> Domain
domainCreate name members =
    {
            name = DomainName name
        ,   members = 
               members 
               |> NList.map (createDomainMember (Factor 1))           
    }

domainAddMember : DomainMember -> Domain ->  Domain
domainAddMember member domain  =
      {  domain | members =  NList.append domain.members [ member ] }

createDimensionWithDefault: Domain -> Dimension
createDimensionWithDefault domain =
    DimensionWithDefault (createDefaultMember (Factor 1) "Total", domain)

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
