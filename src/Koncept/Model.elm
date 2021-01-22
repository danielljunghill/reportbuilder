module Koncept.Model exposing (..)
import Koncept.Cube exposing (..)
import List

-- import Html.Attributes exposing (selected)

type alias Id = String
type alias KonceptInformation = {
    name: String,
    id: Id,
    selected: Bool
  } 


type alias KI = KonceptInformation
type AbstractKoncept = AbstractKoncept KI  
type ValueKoncept = ValueKoncept  KI
akToKi: AbstractKoncept -> KI
akToKi (AbstractKoncept ki) = ki

vkToKi: ValueKoncept -> KI
vkToKi (ValueKoncept ki) = ki



type Koncept   = 
   Value ValueKoncept
   | Koncepts (AbstractKoncept , List Koncept)



konceptToKi: Koncept -> KI
konceptToKi koncept =
  case koncept of
    Koncepts (AbstractKoncept ki,_) -> ki
    Value (ValueKoncept ki) -> ki
     
type ParentKoncept = 
     ParentKoncept Koncept

add: ParentKoncept -> Koncept -> Koncept
add (ParentKoncept parent) koncept =
  case parent of
    Value (ValueKoncept ki) ->  
        Koncepts (AbstractKoncept ki, [ koncept ])
    Koncepts (ki,kl) ->
        Koncepts (ki , List.append kl [ koncept ])

       

copy: Maybe Koncept -> Koncept ->  Maybe Koncept
copy parent state   =
    case state of
    Koncepts (ak,koncepts) ->
        let 
            
            newAk: Maybe Koncept
            newAk =  Koncepts (ak,[]) |> Just
            acc = List.foldl (\s k -> copy k s) newAk koncepts 
        in
          case parent of
           Nothing -> acc 
           Just p -> 
              case acc of
               Nothing -> Just p
               Just child -> Just (add (ParentKoncept p) child)
    Value v ->  
        case parent of 
         Just p -> Just (add (ParentKoncept p) state)
         Nothing -> Just state

-- type SelectedKoncept = SelectedKoncept Koncept
mapInfo: (KonceptInformation -> KonceptInformation) -> Koncept -> Maybe Koncept
mapInfo mapf headKoncept =
  let
      recMap:Maybe Koncept -> Koncept ->  Maybe Koncept 
      recMap parent state   =
        case state of
        Koncepts (AbstractKoncept ki,koncepts) ->
            let 
                newAk: AbstractKoncept
                newAk = mapf ki |> AbstractKoncept
                newKoncept: Maybe Koncept
                newKoncept =  Koncepts (newAk,[]) |> Just
                acc = List.foldl (\s k -> recMap k s) newKoncept koncepts 
            in
              case parent of
              Nothing -> acc 
              Just p -> 
                  case acc of
                  Nothing -> Just p
                  Just child -> Just (add (ParentKoncept p) child)
        Value (ValueKoncept ki) ->  
            let 
              newValueKoncept = mapf ki |> ValueKoncept |> Value
            in
              case parent of 
              Just p -> Just (add (ParentKoncept p) newValueKoncept)
              Nothing -> Just newValueKoncept
  in
      recMap Nothing headKoncept


 
selectSingleKoncept: KonceptInformation -> (KonceptInformation -> KonceptInformation)
selectSingleKoncept info =
    let
       selectInfo: KonceptInformation -> KonceptInformation -> KonceptInformation
       selectInfo sk ki =   { ki | selected = (sk.id == ki.id) }  
    in
       selectInfo info  


select: KonceptInformation -> Koncept -> Maybe Koncept
select ki = mapInfo (selectSingleKoncept ki) 


type alias DimensionalValue =
  {
        koncept: ValueKoncept
      , dimensions: List HyperDimension
  }

type DimensionalKoncept =
    Cube (HyperCube, List Koncept)
    |  Abstract (AbstractKoncept, List DimensionalKoncept)
    |  SingleValue ValueKoncept





  
-- type InsertKoncept = InsertKoncept Koncept
-- type ParentKoncept = ParentKoncept Koncept
-- insert: Koncept -> ParentKoncept -> InsertKoncept -> Koncept
-- insert state (ParentKoncept parent) (InsertKoncept ik)   =   
--     let 
--       recInsert: Koncept -> Maybe Koncept -> Maybe Koncept
--       recInsert state parent =
--         match state with
--         | Koncepts (name,koncepts) ->
--             let valueKoncept = (Koncepts (name,[]))
--             let acc = List.fold (fun state koncept -> add' koncept state) (Some valueKoncept) koncepts        
--             match parent with
--             | None -> 
--                 if (Koncept.name acc.Value = pn) then Koncept.add ik (ParentKoncept acc.Value) else acc.Value
--                 |> Some
--             | Some p -> 
--                 let newAcc = if pn = name then Koncept.add ik (ParentKoncept acc.Value) else acc.Value
--                 Koncept.add newAcc (ParentKoncept p)
--                 |> Some
--         | Value v ->  
--             match parent with 
--             | Some p -> 
--                let newState = if pn = Koncept.name state then Koncept.add ik (ParentKoncept state) else state
--                Koncept.add newState (ParentKoncept p)
--                |> Some
--             | None ->
--                 if pn = Koncept.name state then Koncept.add ik (ParentKoncept state) else state
--                 |> Some
--     in
--         recInsert state Nothing
           












