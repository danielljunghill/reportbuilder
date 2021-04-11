module Koncepts.CubeRow exposing (createIndented,addMembers,KonceptValuePath,KonceptPath(..),CubeRow)
import Koncepts.Model exposing (Member, ValueKoncept, AbstractKoncept, DimensionalKoncept(..))
import Koncepts.Area exposing (..)
import NList exposing (..)
import Lists 

type alias KonceptValuePath =
   {
         value: ValueKoncept
      ,  abstracts: List AbstractKoncept 
   }

konceptValuePath value  =       
    {
            value = value
        ,   abstracts = [] 
    }

type KonceptPath =
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
            konceptPath =  vk |> konceptValuePath |> ValuePath
        ,   members = []
    }

addMembers: NList Member  -> CubeRow -> CubeRow 
addMembers members identity =
    { identity | members = (members |> NList.toList) ++ identity.members}

addAbstractKoncept: AbstractKoncept -> CubeRow -> CubeRow
addAbstractKoncept ak identity =
    let 
        newPath =
            case identity.konceptPath of
                AbstractPath path -> 
                    NList.addList path [ ak ] 
                    |> AbstractPath
                ValuePath path -> 
                    { path | abstracts = [ ak ] ++ path.abstracts } 
                    |> ValuePath
    in
        { identity | konceptPath = newPath }

createIndented: List DimensionalKoncept -> List CubeRow
createIndented dimensionalKoncepts = 
    let 
        recfirst: DimensionalKoncept -> List CubeRow
        recfirst koncept =
            case koncept of
                DimensionalAbstract (ak, koncepts) -> 
                    [ 
                        fromAbstract ak 
                    ] ++ (koncepts |> Lists.collect (recRest ak))
                DimensionalValue vk -> [ fromValue vk ]

        recRest: AbstractKoncept -> DimensionalKoncept -> List CubeRow
        recRest parent koncept =
            case koncept of
                DimensionalAbstract (ak, koncepts) -> 
                    [ 
                        ak
                        |> fromAbstract  
                        |> addAbstractKoncept parent
                    ] ++ (koncepts |> Lists.collect (recRest ak))
                DimensionalValue vk ->
                    [ 
                        fromValue vk
                        |> addAbstractKoncept parent
                    ]
    in 
        dimensionalKoncepts |> Lists.collect recfirst