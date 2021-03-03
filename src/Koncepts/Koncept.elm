module Koncepts.Koncept exposing (..)
import Koncepts.Model exposing (..)
import Id exposing (..)
import ResultExtension exposing (..)
import Result
import List


createValue: String -> Koncept   
createValue = createValueKoncept >> Value

addValue : ValueKoncept -> Koncept -> Result String Koncept
addValue k p =
        case p of
                Cube (hc, koncepts) ->
                        (hc, koncepts ++ [ DimensionalValue k ])
                        |> Cube 
                        |> Result.Ok
                Abstract (ak, koncepts) ->
                        (ak, koncepts ++ [ Value k ])
                        |> Abstract
                        |> Result.Ok
                Value _ ->  Result.Err  "ValueKoncept koncept cannot be added to a ValueKoncept"   


createAbstract: String -> Koncept
createAbstract name =
        (createAbstractKoncept name, []) |> Abstract
    
addAbstract: AbstractKoncept -> Koncept -> Result String Koncept
addAbstract k p =
        case p of
         Cube (hc,koncepts) ->
                (hc, koncepts ++ [ DimensionalAbstract (k,[]) ])
                |> Cube
                |> Result.Ok
         Abstract (ak,koncepts) -> 
                (ak, koncepts ++ [ Abstract (k,[]) ])
                |> Abstract
                |> Result.Ok
         Value _ -> Result.Err  "An abstract koncept cannot be added to a value koncept"   


addCube: HyperCube -> Koncept -> Result String Koncept
addCube hc p =
        case p of
           Abstract (ak,koncepts) ->
                (ak, koncepts ++ [  (hc,[]) |> Cube ]) 
                |> Abstract
                |> Result.Ok
           Cube _  ->
                "Hypercube cannot be added to a Hypercube" 
                |> Err 
           Value _  ->
                "Hypercube cannot be added to a Koncept of type Value" 
                |> Err 

type ParentKoncept =  ParentKoncept Koncept

add: Koncept -> ParentKoncept -> Result String Koncept
add koncept (ParentKoncept parent) =
   case parent of
   Abstract (ak, koncepts) ->  (ak , koncepts ++ [ koncept]) |> Abstract |> Ok
   Cube _  -> Err "Only a dimensional koncept kan be added to a hyper cube"
   Value _  -> Err "Value cannot act as parent for koncept"

maybeAdd:Maybe Koncept -> Maybe ParentKoncept -> Result String (Maybe Koncept)
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

andThenMaybeAdd: Result String (Maybe ParentKoncept) -> Maybe Koncept -> Result String (Maybe Koncept)
andThenMaybeAdd parent koncept =
   Result.andThen (maybeAdd koncept) parent

mapToParent: Result String (Maybe Koncept) -> Result String (Maybe ParentKoncept)
mapToParent m =
   m |> Result.map (\r -> r |> Maybe.map ParentKoncept)
parentAsKoncept: Result String (Maybe ParentKoncept) -> Result String (Maybe Koncept)
parentAsKoncept parent  =
        parent |> Result.map (\r -> r |> Maybe.map (\ (ParentKoncept k) -> k))

recursivefold: (Koncept -> Result String (Maybe Koncept)) ->  Result String (Maybe ParentKoncept) ->  Result String (Maybe Koncept) ->  Result String (Maybe ParentKoncept)
recursivefold f p k  =
   let 

      -- first: Result String (Maybe Koncept)
      -- first = f k
      fmap: Result String (Maybe ParentKoncept) -> Maybe Koncept -> Result String (Maybe ParentKoncept)
      fmap parent koncept =
            case koncept of
               Just ki ->
                  case ki of
                     Abstract (ak, koncepts) ->
                        let 
                           newKoncept: Result String (Maybe ParentKoncept)
                           newKoncept = (ak, []) |> Abstract |> ParentKoncept |> Just |> Ok
                           accKoncept:Result String (Maybe Koncept)   
                           accKoncept = 
                              koncepts 
                              |> List.map f 
                              |> List.foldl (\a b -> recursivefold f b a) newKoncept 
                              |> parentAsKoncept
                        in
                           accKoncept |> Result.andThen (andThenMaybeAdd parent)  
                           |> mapToParent                   
                     Value (_) -> 
                           Result.andThen (andThenMaybeAdd parent) (ki |> Just |> Ok) 
                           |> mapToParent
                     Cube (_)->
                           Result.andThen (andThenMaybeAdd parent) (ki |> Just |> Ok) 
                           |> mapToParent
               Nothing -> parent
   in
      Result.andThen (fmap p) k

fold: (Koncept -> Result String (Maybe Koncept)) -> Koncept -> Result String Koncept
fold f m =
   let 
       first: Result String (Maybe Koncept)
       first = f m
   in
      recursivefold f (Ok Nothing) first
      |> parentAsKoncept
      |> Result.map (\ v -> case v of 
                              Just vi -> Ok vi 
                              Nothing -> Err "Empty result from fold of koncept")
      |> ResultExtension.foldOne



type alias KonceptAction = ModelAction Koncept

actionToKonceptOption: Koncept -> KonceptAction -> Maybe Koncept
actionToKonceptOption koncept action =
      case action of
       Delete -> Nothing
       MapValue k -> Just k
       Ignore -> Just koncept

mapCube: (HyperCube -> List DimensionalKoncept -> Result String KonceptAction) -> Koncept -> Result String (Maybe Koncept)
mapCube f koncept =
    case koncept of
      Cube (hc,koncepts) ->
            f hc koncepts 
            |> Result.map (actionToKonceptOption koncept)
      _ -> koncept |> Just |> Ok 

mapAbstractKoncept: (AbstractKoncept -> List Koncept -> Result String KonceptAction) -> Koncept -> Result String (Maybe Koncept)
mapAbstractKoncept f koncept =
   case koncept of
     Abstract (ak,koncepts) ->
        f ak koncepts
        |> Result.map (actionToKonceptOption koncept)
     _ -> koncept |> Just |> Ok 


mapValueKoncept: (ValueKoncept ->  Result String KonceptAction) -> Koncept -> Result String (Maybe Koncept)
mapValueKoncept f koncept =
   case koncept of
      Value vk ->
         f vk
         |> Result.map (actionToKonceptOption koncept)
      _ -> koncept |> Just |> Ok 

