module Koncepts.Hypercube exposing (..)
import Id exposing (..)
import ResultExtension exposing (..)
import Koncepts.Model exposing (..)
import NList exposing(..)
import NList as NList
-- type DomainName = DomainName String
-- type DomainMember = DomainMember String
-- type alias Domain =
--     {
--             name: DomainName
--         ,   members: List DomainMember 
--     }

-- type DefaultMember = DefaultMember String  
-- type Dimension =
--      DimensionWithDefault  (DefaultMember,Domain)
--      | DimensionWithoutDefault Domain

-- type HyperDimension =
--      Opened Dimension
--      | Closed  Dimension

-- type HyperCubeName = HyperCubeName String
-- type HyperCubeId = HyperCubeId Id
-- type alias HyperCube =
--     {
--             name: HyperCubeName
--         ,   head: HyperDimension
--         ,   tail: List HyperDimension 
--         ,   id: HyperCubeId
--     }

domainCreate: String -> NList String -> Domain
domainCreate name members =
    {
            name = DomainName name
        ,   members = 
               members 
               |> NList.map (createDomainMember (Factor 1))
            
    }

-- domainCreate : Member -> DomainName -> Domain
-- domainCreate member name =
--    {     name = name
--       ,  members = NList.create member}

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

        

