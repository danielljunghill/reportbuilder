module Koncepts.CubeKoncept exposing (..)
import Koncepts.Model exposing(createValueKoncept, ValueKoncept, createAbstractKoncept)
import Koncepts.Model exposing (..)
import ResultExtension
import Koncepts.Area exposing (..)
import Koncepts.Area as Area
import Lists
import Prime exposing (..)



createValue: String -> Prime -> PrimeResult DimensionalKoncept   
createValue name prime  = 
   let
      koncept: DimensionalKoncept
      koncept =
         prime
         |> factorFromPrime
         |> createValueKoncept name 
         |> DimensionalValue
   in 
      prime
      |> generatePrime 
      |> createPrimeResult koncept 


addValueKoncept:  ValueKoncept -> DimensionalKoncept -> Result String DimensionalKoncept
addValueKoncept vk parent = 
   case parent of
      DimensionalAbstract (ak, d) -> (ak, d ++ [ (vk |> DimensionalValue) ]) |> DimensionalAbstract|> Ok
      DimensionalValue _ -> Err "A Value koncept cannot be added to a dimensional value"


createAbstract: List DimensionalKoncept  -> String -> DimensionalKoncept
createAbstract dims  = createAbstractKoncept >>  (\ak -> DimensionalAbstract (ak,dims))

type ParentKoncept =  ParentKoncept DimensionalKoncept

add: DimensionalKoncept -> ParentKoncept -> Result String DimensionalKoncept
add koncept (ParentKoncept parent) =
   case parent of
      DimensionalAbstract (ak, koncepts) ->
         (ak,koncepts ++ [koncept]) 
         |> DimensionalAbstract
         |> Ok
      _ -> Err "Parent must be an abstract dimensional koncept"


maybeAdd:Maybe DimensionalKoncept -> Maybe ParentKoncept -> Result String (Maybe DimensionalKoncept)
maybeAdd koncept parent  =
   case parent of
      Nothing -> koncept |> Ok
      Just (ParentKoncept pk) -> 
         case koncept of
            Just child -> 
               pk
               |> ParentKoncept
               |> add child
               |> Result.map Just
            Nothing ->  
               pk |> Just |> Ok

andThenMaybeAdd: Result String (Maybe ParentKoncept) -> Maybe DimensionalKoncept -> Result String (Maybe DimensionalKoncept)
andThenMaybeAdd parent koncept =
   Result.andThen (maybeAdd koncept) parent

mapToParent: Result String (Maybe DimensionalKoncept) -> Result String (Maybe ParentKoncept)
mapToParent m =
   m |> Result.map (\r -> r |> Maybe.map ParentKoncept)
parentAsKoncept: Result String (Maybe ParentKoncept) -> Result String (Maybe DimensionalKoncept)
parentAsKoncept parent  =
        parent |> Result.map (\r -> r |> Maybe.map (\ (ParentKoncept k) -> k))

recursivefold: (DimensionalKoncept -> Result String (Maybe DimensionalKoncept)) ->  Result String (Maybe ParentKoncept) ->  Result String (Maybe DimensionalKoncept) ->   Result String (Maybe ParentKoncept)
recursivefold f p k  =
   let 
      fmap: Result String (Maybe ParentKoncept) -> Maybe DimensionalKoncept -> Result String (Maybe ParentKoncept)
      fmap parent koncept =
         case koncept of
            Just ki ->
               case ki of
                  DimensionalAbstract (ak, koncepts) ->
                     let 
                        newKoncept: Result String (Maybe ParentKoncept)
                        newKoncept = (ak, []) |> DimensionalAbstract |> ParentKoncept |> Just |> Ok
                        accKoncept:Result String (Maybe DimensionalKoncept)   
                        accKoncept = 
                           koncepts 
                           |> List.map f 
                           |> List.foldl (\a b -> recursivefold f b a) newKoncept 
                           |> parentAsKoncept
                     in
                        accKoncept |> Result.andThen (andThenMaybeAdd parent)  
                        |> mapToParent                   
                  DimensionalValue (_) -> 
                     let
                        newKoncept: Result String (Maybe DimensionalKoncept)  
                        newKoncept = f ki
                     in
                        Result.andThen (andThenMaybeAdd parent) newKoncept
                        |> mapToParent

            Nothing -> parent
   in
      Result.andThen (fmap p) k

fold: (DimensionalKoncept -> Result String (Maybe DimensionalKoncept)) -> Result String DimensionalKoncept -> Result String DimensionalKoncept
fold f m =
   recursivefold f (Ok Nothing) (m |> Result.map Just)
   |> parentAsKoncept
   |> Result.map (\ v -> case v of 
                           Just vi -> Ok vi 
                           Nothing -> Err "Empty result from fold of koncept")
   |> ResultExtension.foldOne

type KonceptRowItem =
   AbstractRow AbstractKoncept
   | ValueRow ValueKoncept

tryGetValueKoncept kri =
   case kri of
      AbstractRow _ -> Nothing
      ValueRow vk -> Just vk

konceptRowFactor: KonceptRowItem -> Maybe Factor
konceptRowFactor item =
   case item of
      AbstractRow _ -> Nothing
      ValueRow vk -> Just vk.factor

konceptRowItemName: KonceptRowItem -> String
konceptRowItemName item =    
   case item of
      AbstractRow ak -> abstractKonceptNameToString ak.name
      ValueRow vk -> valueKonceptNameToString vk.name





type alias KonceptRow = 
   {
         area: Area
      ,  item: KonceptRowItem
   }

createAbstractRow: Area -> AbstractKoncept -> KonceptRow
createAbstractRow area item =
   {
            area = area
        ,   item = AbstractRow item  
   }
createValueRow: Area -> ValueKoncept -> KonceptRow
createValueRow area item =
      {
            area = area
        ,   item = ValueRow item  
      }

indentedRowSpan: DimensionalKoncept -> List Int
indentedRowSpan dimKoncept =
   let 
      recCalcForOne: Int -> DimensionalKoncept -> List Int
      recCalcForOne stateOne koncept =
         let
            newStateOne: Int
            newStateOne = 1
         in
            case koncept of
               DimensionalAbstract (_,childKoncepts) ->
                  let
                     recCalcForMany: Int -> List DimensionalKoncept -> List Int
                     recCalcForMany stateMany koncepts =
                        case koncepts of
                           [] -> [ stateMany ]
                           head :: tail ->  (recCalcForOne stateMany head) ++ recCalcForMany stateMany tail
                  in
                     recCalcForMany newStateOne childKoncepts
               DimensionalValue _ -> [ newStateOne ]
     
   in
      recCalcForOne 0 dimKoncept
      

calculateIndentedRows: List DimensionalKoncept -> List KonceptRow
calculateIndentedRows koncepts =
   let
      maxSpan: Int
      maxSpan = 
         koncepts 
         |> Lists.collect indentedRowSpan 
         |> Lists.maxInt
         |> Maybe.withDefault 0

      area: VerticalStart -> HorizontalStart -> Area
      area vStart hStart  = 
         { 
               horizontalStart =  hStart
            ,  horizontalSpan = 
                  maxSpan - (Area.horizontalStartToInt hStart) + 1
                  |> Span 
                  |> HorizontalSpan
            ,  verticalSpan = Area.oneVerticalSpan
            ,  verticalStart = vStart
         }
    in
      let
          recCalculateRows: HorizontalStart -> DimensionalKoncept-> List KonceptRow
          recCalculateRows horizontalStart koncept =
            case koncept of
               DimensionalAbstract (ak, childKoncepts) -> 
                  [  
                     createAbstractRow (area Area.oneVerticalStart horizontalStart) ak
                  ] 
                  ++ (childKoncepts |> Lists.collect (recCalculateRows (Area.horizontalStartMap startIncrement horizontalStart))) 
               DimensionalValue vk -> 
                  [
                     createValueRow (area Area.oneVerticalStart horizontalStart) vk                     
                  ] 

      in  
  
         koncepts 
         |> Lists.collect (recCalculateRows Area.oneHorizontalStart)
         |> Lists.mapi (\i row -> { row | area = row.area |> Area.setVerticalStart (Start (i + 1)) })  


type CubeColumnOffset = CubeColumnOffset Offset

cubeColumnOffsetToOffset: CubeColumnOffset -> Offset
cubeColumnOffsetToOffset (CubeColumnOffset offset) = offset
 
type alias CubeRows =
   {
         rows: List KonceptRow
      ,  offset: CubeColumnOffset

   }

calculateIndentedCubeRows: List DimensionalKoncept  -> CubeRows
calculateIndentedCubeRows koncepts =
   let 
      offset: Offset
      offset =
            {
                  verticalStart = zeroVerticalStart
               ,  horizontalStart = oneHorizontalStart
            } 
   in
      {
            rows = calculateIndentedRows koncepts
         ,  offset = offset |> CubeColumnOffset
      }

-- let calcSpan koncept =
--    let rec span state (koncept: DimensionalKoncept) = 
--       let newState = state + 1
--       match koncept with
--       | DimensionalAbstract (_, koncepts) -> 
--            let rec span' state koncepts =
--                match koncepts with
--                | [] -> [ state ]
--                | head :: tail ->    
--                   span state head
--                   @ span' state tail
--            span' newState koncepts
--       | DimensionalValue _ ->
--             [ newState ] 
--    span 0 koncept 