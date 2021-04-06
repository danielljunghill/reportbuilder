module Koncepts.CubeKonceptIdentity exposing (createForIndentedTable,addMembers,KonceptValuePath,KonceptPath,KonceptMemberIdentity)
import Koncepts.Model exposing (Member, ValueKoncept, AbstractKoncept)
import Koncepts.Area exposing (..)
import NList exposing (..)

type alias KonceptValuePath =
   {
         value: ValueKoncept
      ,  abstracts: AbstractKoncept
   }

type alias KonceptPath =
      AbstractPath (NList AbstractKoncept)
      | ValuePath KonceptValuePath

type alias CubeRow =
   {
         konceptPath: KonceptPath    
      ,  members: List Member 
   }


fromAbstract: AbstractKoncept -> CubeRow
fromAbstract ak  =
    {
            konceptPath = ak  |> NList.create |> AbstractPath
        ,   members = []
    }

fromValue:ValueKoncept -> CubeRow
fromValue vk =
    {
        konceptPath = { value = vk; abstracts = [] } |> ValuePath
        members = []
    }

addMembers: NList Member  -> CubeRow -> CubeRow 
addMembers members identity=
    { identity | members = NList.toList members @ identity.members}

addAbstractKoncept: AbstractKoncept -> CubeRow -> CubeRow
addAbstractKoncept ak identity =
    let 
        newPath =
            case identity.konceptPath of
                AbstractPath path -> 
                    NList.addList path [ ak ] 
                    |> AbstractPath
                ValuePath path -> 
                    { path | abstracts = [ ak ] @ path.Abstracts } 
                    |> ValuePath
    in
        { identity | KonceptPath = newPath }

createForIndentedTable: List DimensionalKoncept -> List CubeRow
createForIndentedTable koncepts = 
    let 
        recfirst: DimensionalKoncept -> List CubeRow
        recfirst koncept =
            case koncept of
                DimensionalAbstract (ak, koncepts) -> 
                    [ 
                        fromAbstract ak 
                    ] @ (koncepts |> List.collect (recRest ak))
                DimensionalValue vk -> [ fromValue vk ]

        recRest: AbstractKoncept -> DimensionalKoncept -> List CubeRow
        recRest parent koncept =
            case koncept of
                DimensionalAbstract (ak, koncepts) -> 
                    [ 
                        ak
                        |> fromAbstract  
                        |> addAbstractKoncept parent
                    ] @ (koncepts |> List.collect (rest ak))
            | DimensionalValue vk ->
                [ 
                    fromValue vk
                    |> addAbstractKoncept parent
                ]
    in 
        koncepts |> List.collect recfirst