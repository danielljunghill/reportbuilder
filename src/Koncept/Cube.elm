module Koncept.Cube exposing (..)

type  DomainName = DomainName String
type  DomainMember = DomainMember String
type DefaultMember = DefaultMember String  

type alias Domain =
    {
            name: DomainName
        ,   members: List DomainMember
    }
createEmptyDomain: DomainName -> Domain
createEmptyDomain domainName = { name = domainName, members = []}

addDomainMember: Domain -> DomainMember -> Domain
addDomainMember domain member = { domain | members = List.append domain.members [ member ] }

type Dimension =
     DimensionWithDefault (DefaultMember, Domain)
     | DimensionWithoutDefault Domain
     
type HyperDimension =
     Opened Dimension
     | Closed Dimension

type alias HyperCubeName = String

type alias HyperCube =
    {
            name: HyperCubeName
        ,   head: HyperDimension
        ,   tail: List HyperDimension
    }


createHyperCube: HyperCubeName -> HyperDimension -> HyperCube
createHyperCube hyperCubeName hyperDimension =
    {
        name = hyperCubeName
        ,   head = hyperDimension
        ,   tail = []
    }

addDimension: HyperCube -> HyperDimension -> HyperCube
addDimension cube dimension = { cube | tail = List.append cube.tail [ dimension ]}
hyperCubeDimensions: HyperCube -> List HyperDimension
hyperCubeDimensions cube =
    [ cube.head ] ++ cube.tail




