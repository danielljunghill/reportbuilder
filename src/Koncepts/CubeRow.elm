module Koncepts.CubeRow exposing (createIndented,addMembers,KonceptValuePath,KonceptPath(..),CubeRow,CubeRowMembers(..))
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


type CubeRowMembers = CubeRowMembers (List Member)

type alias CubeRow =
   {
         konceptPath: KonceptPath    
      ,  members: CubeRowMembers
   }

type alias CubeRowContext =     
    {
        abstracts: Abstract List
        members: Member List
    }

type CubeValueRow = CubeValueRow (ValueKoncept,CubeRowContext)
type CubeAbstractRow = CubeAbstractRow (AbstractKoncept,CubeRowContext)
type CubeRow =
    | AbstractRow CubeAbstractRow 
    | ValueRow CubeValueRow


fromAbstract: AbstractKoncept -> CubeRow
fromAbstract ak  =
    {
            konceptPath = ak  |> NList.create |> AbstractPath
        ,   members = CubeRowMembers []
    }

fromValue:ValueKoncept -> CubeRow
fromValue vk =
    {
            konceptPath =  vk |> konceptValuePath |> ValuePath
        ,   members = CubeRowMembers []
    }

addMembers: NList Member  -> CubeRow -> CubeRow 
addMembers members cubeRow =
    let 
        (CubeRowMembers cubeRowMembers) = cubeRow.members
    in
    { cubeRow | members = ((members |> NList.toList) ++ cubeRowMembers) |> CubeRowMembers}

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