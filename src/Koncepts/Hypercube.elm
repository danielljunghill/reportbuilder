module Koncepts.Hypercube exposing (..)
import Id exposing (..)
import ResultExtension exposing (..)
import Koncepts.Model exposing (..)


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

domainCreate: String -> List String -> Domain
domainCreate name members =
    {
            name = DomainName name
        ,   members = members |> List.map DomainMember
    }

domainCreateEmpty : DomainName -> Domain
domainCreateEmpty name =
   {     name = name
      ,  members = [] }

domainAddMember : DomainMember -> Domain ->  Domain
domainAddMember member domain  =
      {  domain | members = domain.members ++ [ member ] }



createDimensionWithDefault: Domain -> Dimension
createDimensionWithDefault domain =
    DimensionWithDefault (DefaultMember "Total", domain)

createDimensionWithoutDefault: Domain -> Dimension
createDimensionWithoutDefault domain =
    DimensionWithoutDefault domain

create: String -> HyperDimension -> HyperCube
create name dimension =
        {
                        id = Id.create() |> HyperCubeId
                ,       name = HyperCubeName name
                ,       head = dimension
                ,       tail = []
        }

addDimension: HyperCube -> HyperDimension -> HyperCube
addDimension hyperCube dimension =
   { hyperCube | tail = hyperCube.tail ++ [ dimension ]}

        

