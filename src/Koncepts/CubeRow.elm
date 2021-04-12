module Koncepts.CubeRow exposing (createIndented,addMembers, addAbstractKoncept, CubeRow(..), CubeRowContext, CubeValueRow, CubeAbstractRow)
import Koncepts.Model exposing (Member, ValueKoncept, AbstractKoncept, DimensionalKoncept(..))
import Koncepts.Area exposing (..)
import NList exposing (..)
import Lists 

-- type alias KonceptValuePath =
--    {
--          value: ValueKoncept
--       ,  abstracts: List AbstractKoncept 
--    }

-- konceptValuePath value  =       
--     {
--             value = value
--         ,   abstracts = [] 
--     }

-- type KonceptPath =
--       AbstractPath (NList AbstractKoncept)
--       | ValuePath KonceptValuePath


-- type CubeRowMembers = CubeRowMembers (List Member)

-- type alias CubeRow =
--    {
--          konceptPath: KonceptPath    
--       ,  members: CubeRowMembers
--    }

type alias CubeRowContext =     
    {
            abstracts: List AbstractKoncept 
        ,   members: List Member 
    }

emptyCubeRowContext =
    {
            abstracts = []
        ,   members = [] 
    }

addMembersToContext: CubeRowContext -> List Member -> CubeRowContext
addMembersToContext context members  =
    { context | members = context.members ++ members }

addAbstractsToContext:  CubeRowContext ->  AbstractKoncept -> CubeRowContext
addAbstractsToContext context ak  =
    { context | abstracts = context.abstracts ++ [ ak ] }

type CubeValueRow = CubeValueRow (ValueKoncept,CubeRowContext)
type CubeAbstractRow = CubeAbstractRow (AbstractKoncept,CubeRowContext)
type CubeRow =
    AbstractRow CubeAbstractRow 
    | ValueRow CubeValueRow


-- fromAbstract: AbstractKoncept -> CubeRow
-- fromAbstract ak  =
--     {
--             konceptPath = ak  |> NList.create |> AbstractPath
--         ,   members = CubeRowMembers []
--     }

fromAbstract ak =  (ak, emptyCubeRowContext) |> CubeAbstractRow |> AbstractRow
    
fromValue vk =  (vk, emptyCubeRowContext) |> CubeValueRow |> ValueRow
           
    
-- fromValue:ValueKoncept -> CubeRow
-- fromValue vk =
--     {
--             konceptPath =  vk |> konceptValuePath |> ValuePath
--         ,   members = CubeRowMembers []
--     }

addMembers: NList Member  -> CubeRow -> CubeRow 
addMembers members cubeRow =

    case cubeRow of
        AbstractRow (CubeAbstractRow (ak, context)) ->  
            (ak, members |> NList.toList |> addMembersToContext context)
            |> CubeAbstractRow 
            |> AbstractRow
        ValueRow (CubeValueRow (ak, context)) -> 
            (ak, members |> NList.toList |> addMembersToContext context) 
            |> CubeValueRow 
            |> ValueRow

addAbstractKoncept: AbstractKoncept -> CubeRow -> CubeRow
addAbstractKoncept newAk cubeRow =
     case cubeRow of
        AbstractRow (CubeAbstractRow (ak, context)) ->  
            (ak, newAk |> addAbstractsToContext context)
            |> CubeAbstractRow 
            |> AbstractRow
        ValueRow (CubeValueRow (ak, context)) -> 
            (ak, newAk |> addAbstractsToContext context) 
            |> CubeValueRow 
            |> ValueRow

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