module Koncepts.Koncept exposing (..)
import Koncepts.Model exposing (..)
import Id exposing (..)
import ResultExtension exposing (..)
import Result


createValue: String -> ValueKoncept   
createValue name  =
        {
                id = Id.create() |> ValueKonceptId 
            ,   name = name |> ValueKonceptName }

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


createAbstract: String -> AbstractKoncept
createAbstract name =
        {
                id = Id.create() |> AbstractKonceptId 
                , name = AbstractKonceptName name                      
        }       

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
         Value _ -> Result.Err  "Abstract koncept cannot be added to a ValueKoncept"   

  

createCube: String -> HyperDimension -> HyperCube
createCube name dimension =
        {
                        id = Id.create() |> HyperCubeId
                ,       name = HyperCubeName name
                ,       head = dimension
                ,       tail = []
        } 
        

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
         Cube _  -> Err "Only dimensional koncepts kan be added to a HyperCube"
         Value _  -> Err "Value cannot act as parent for koncept"

map:(Koncept -> Result String Koncept) -> Result String Koncept -> Result String Koncept
map f m =
   let 
      innerMap: Result String (Maybe Koncept) -> Result String Koncept -> Result String (Maybe Koncept)
      innerMap parent koncept = 
         let 
            addKoncept: Koncept -> Result String (Maybe Koncept)
            addKoncept k =
               case k of
                  Abstract (ak,koncepts) -> Err "Not implemented"
                  Value vk -> Err "Not implemented"
                  Cube (hc,koncepts) -> Err "Not implemented"
         in
            Result.andThen addKoncept koncept
   in
      let
         parent:  Result String (Maybe Koncept)
         parent = Result.Err "adlsadsndl"
      in

         innerMap parent m
         |> Result.map (\v -> 
            case v of 
             Just vi -> Ok vi 
             Nothing -> Err "Empty result")
         |> ResultExtension.foldOne





--     let map (f: Koncept -> Result<_,_>) koncept =
--         let rec map' parent koncept  =
--             let fmap koncept =
--                 match koncept with
--                 | Koncept.AbstractKoncept (ak, koncepts) ->
--                     let newKoncept = (ak, []) |> Koncept.AbstractKoncept |> Some |> Ok
--                     let accKoncept = koncepts |> List.map f |> List.fold map' newKoncept 
--                     let fn1 parent acc =
--                         let fn2 acc p =
--                             match p with
--                             | None -> acc |> Ok
--                             | Some parentKoncept -> 
--                                 match acc with
--                                 | Some childKoncept -> 
--                                     parentKoncept
--                                     |> ParentKoncept 
--                                     |> add childKoncept
--                                 | None ->  parentKoncept |> Ok
--                                 |> Result.map Some
--                         Result.bind (fn2 acc) parent
--                     accKoncept |> Result.bind (fn1 parent)
--                 | Koncept.ValueKoncept vk -> 
--                     let fn parent =
--                         match parent with
--                         | Some p -> ValueKoncept.addToKoncept vk p
--                         | None ->  vk |> Koncept.ValueKoncept |> Ok
--                         |> Result.map Some
--                     parent |> Result.bind fn
--                 | Koncept.Cube (hc,koncepts)->
--                     let newKoncept = f koncept
--                     let fn (parent: Koncept option) =
--                         match parent with
--                             | Some p -> 
--                                 let f koncept = add koncept (ParentKoncept p) 
--                                 Result.bind f newKoncept
--                             | None -> newKoncept 
--                         |> Result.map Some
--                     Result.bind fn parent
--             Result.bind fmap koncept
--         map' (Ok None) koncept
--         |> Result.map (fun v -> match v with | Some vi -> Ok vi | None -> Error "Empty result from map")
--         |> Result.join


-- module Koncept =
--     let createAbstract name =
--         (AbstractKoncept.create name, []) |> Koncept.AbstractKoncept
--     let createValue =
--         ValueKoncept.create >> Koncept.ValueKoncept
--     type ParentKoncept = ParentKoncept of Koncept


--     let iter (koncept: Koncept) =
--         let rec map' (parent: Result<Koncept Option, String>) (koncept: Koncept) : Result<Koncept Option, String> =
--             match koncept with
--             | Koncept.AbstractKoncept (ak, koncepts) ->
--                 let newKoncept = (ak, []) |> Koncept.AbstractKoncept |> Some |> Ok
--                 let accKoncept = koncepts |> List.fold map' newKoncept 
--                 let fn1 (parent: Result<Koncept option, string>) (acc:Koncept option) =
--                     let fn2 (acc: Koncept option) (p: Koncept option)  =
--                         match p with
--                         | None -> acc |> Ok
--                         | Some parentKoncept -> 
--                             match acc with
--                             | Some childKoncept -> 
--                                 parentKoncept
--                                 |> ParentKoncept 
--                                 |> add childKoncept
--                             | None ->  parentKoncept |> Ok
--                             |> Result.map Some
--                     Result.bind (fn2 acc) parent
--                 accKoncept |> Result.bind (fn1 parent)
--             | Koncept.ValueKoncept vk ->
--                 let fn (parent: Koncept option) =
--                     match parent with
--                     | Some p -> ValueKoncept.addToKoncept vk p
--                     | None ->  vk |> Koncept.ValueKoncept |> Ok
--                     |> Result.map Some
--                 parent |> Result.bind fn

--             | Koncept.Cube (hc,koncepts)->
--                 Error "Handling of Cube not implemented"
--         map' (Ok None) 



     
-- //  open Koncept
-- //  open AbstractKoncept
-- //  open ValueKoncept

-- let a1 = 
--     "Head abstract1"  
--     |> Koncept.createAbstract 
--     |> Koncept.ParentKoncept  
--     |> Koncept.add ("Sub abstract2" |> Koncept.createAbstract)

-- let v1 = Koncept.createValue "First Values"
-- let added =
--     a1
--     |> Result.map Koncept.ParentKoncept 
--     |> Result.bind (Koncept.add v1)

-- // let mapped = 
-- //     added 
-- //     |> Result.bind (Koncept.map )

-- let mapKoncept (koncept: Koncept) =
--     match koncept with
--     | Koncept.AbstractKoncept (ak,koncepts) ->
--         if ak.Name = AbstractKonceptName "Sub abstract2" then
--             Koncept.AbstractKoncept (ak, koncepts @ ([ "ett jävla value" |> Koncept.createValue ]))
--         else
--             koncept 
--     | _ -> koncept
--     |> Ok

-- let added2 = Koncept.map mapKoncept added
-- let added3 = Koncept.map mapKoncept added2

-- let optionKoncept (koncept: Koncept) (konceptAdd: Koncept option) =
--     match konceptAdd with
--     | Some k -> k
--     | None -> koncept
--     |> Ok

-- let mapCube (f:HyperCube -> DimensionalKoncept List-> Result<Koncept option,_>) (koncept: Koncept) =
--     match koncept with
--     | Koncept.Cube (hc,koncepts) ->
--         f hc koncepts
--         |> Result.bind (optionKoncept koncept)
--     | _-> koncept |> Ok
   

-- let mapAbstractKoncept (f:AbstractKoncept -> Koncept List -> Result<Koncept option,_>) (koncept: Koncept) =
--     match koncept with
--     | Koncept.AbstractKoncept (ak,koncepts) ->
--         f ak koncepts
--         |> Result.bind (optionKoncept koncept) 
--     | _-> koncept |> Ok
 

-- let mapValueKoncept (f:ValueKoncept -> Result<Koncept option,_>) (koncept: Koncept) =
--     match koncept with
--     | Koncept.ValueKoncept vk ->
--         f vk
--         |> Result.bind (optionKoncept koncept) 
--     | _-> koncept |> Ok


-- let addCube  =
--     let hyperDimension = 
--         ["kv1"; "kv2"; "kv3";"kv4"]
--         |> Domain.create "Kvartal" 
--         |> Dimension.createWithDefault
--         |> Closed
--         |> HyperCube.create "Kvartal och annat"  

--     let f ak koncepts =
--         if ak.Name = AbstractKonceptName "Sub abstract2" then
--             (ak, koncepts @ ([ Koncept.Cube (hyperDimension, []) ])) 
--             |> Koncept.AbstractKoncept 
--             |> Some
--         else
--             None
--         |> Ok

--     mapAbstractKoncept f 
--     >> Result.mapError (fun err -> sprintf "%A" err)
--     // |> Result.mapError (fun err -> sprintf "%A" err)


-- let added4 = Koncept.map addCube added3

-- let addDimensionalKoncept  =
--     let dimKoncepts = [ DimensionalKoncept.create "Intäkter" ; DimensionalKoncept.create "Försäljning cyklar"; DimensionalKoncept.create "Bidrag" ]
--     let f (hc: HyperCube) dimension =
--         if hc.Name = HyperCubeName "Kvartal och annat" then
--             (hc,dimKoncepts) |> Koncept.Cube |> Some
--         else
--             None 
--         |> Ok
--     mapCube f 

-- let added5 = Koncept.map addDimensionalKoncept added4
-- //add


-- let addValue  =
--     let f (ak:AbstractKoncept) koncepts =
--        if ak.Name = AbstractKonceptName "Head abstract1" then
--             (ak, koncepts @ [ "dagen d" |> ValueKoncept.create |> Koncept.ValueKoncept]) |> Koncept.AbstractKoncept |> Some
--        else
--             None
--        |> Ok
--     mapAbstractKoncept f  
  

-- let added6 = Koncept.map addValue  added5 