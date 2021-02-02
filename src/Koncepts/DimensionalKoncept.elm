module Koncepts.Dimensionalkoncept exposing (..)
import Koncepts.Model exposing(createValueKoncept, ValueKoncept, createAbstractKoncept)
import Koncepts.Model exposing (DimensionalKoncept(..))
import ResultExtension


createValue: String -> DimensionalKoncept
createValue = createValueKoncept >> DimensionalValue

addValueKoncept:  ValueKoncept -> DimensionalKoncept -> Result String DimensionalKoncept
addValueKoncept vk parent = 
   case parent of
      DimensionalAbstract (ak, d) -> (ak, d ++ [ (vk |> DimensionalValue) ]) |> DimensionalAbstract|> Ok
      DimensionalValue _ -> Err "A Value koncept cannot be added to a dimensional value"


createAbstract: String -> DimensionalKoncept
createAbstract  = createAbstractKoncept >>  (\ak -> DimensionalAbstract (ak,[]))

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

