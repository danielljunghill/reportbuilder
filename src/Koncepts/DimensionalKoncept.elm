module Koncepts.DimensionalKoncept exposing (..)

--     let addToDimensionalKoncept (vk: ValueKoncept) (parent: DimensionalKoncept) = 
--         match parent with
--         | DimensionalKoncept.AbstractKoncept (ak, d) -> (ak, d @ [ (vk |> DimensionalKoncept.ValueKoncept) ]) |> DimensionalKoncept.AbstractKoncept|> Ok
--         | DimensionalKoncept.ValueKoncept _ -> Error (sprintf "ValueKoncept koncept cannot be added to %A" parent)

--     // let addtoPage 


--     let addToDimensionalKoncept (ak: AbstractKoncept) (parent: DimensionalKoncept) = 
--         match parent with
--         | DimensionalKoncept.AbstractKoncept (ak, koncepts) -> (ak, koncepts @ [  DimensionalKoncept.AbstractKoncept (ak, []) ]) |> DimensionalKoncept.AbstractKoncept |> Ok
--         | DimensionalKoncept.ValueKoncept _ -> Error (sprintf "AbstractKoncept koncept cannot be added to %A" parent)
 module HyperCube =


--     let addDimension hc dimension =
--         { hc with Tail = hc.Tail @ [ dimension ]}



-- module DimensionalKoncept =
--         type ParentDimensionalKoncept= | ParentDimensionalKoncept of DimensionalKoncept

--         let create = ValueKoncept.create >> DimensionalKoncept.ValueKoncept
--         let add (koncept: DimensionalKoncept) (ParentDimensionalKoncept parent) =
--             match koncept with
--             | DimensionalKoncept.AbstractKoncept (ak,koncepts) -> 
--                 (ak, koncepts @ [ koncept])
--                  |> DimensionalKoncept.AbstractKoncept|> Ok
--             | _ -> Error "cannot add"
--         let map f koncept =
--             let rec map' parent koncept  =
--                 let fmap koncept =
--                     match koncept with
--                     | DimensionalKoncept.AbstractKoncept (ak, koncepts) ->
--                         let newKoncept = (ak, []) |> DimensionalKoncept.AbstractKoncept |> Some |> Ok
--                         let accKoncept = koncepts |> List.map f |> List.fold map' newKoncept 
--                         let fn1 parent acc =
--                             let fn2 acc p =
--                                 match p with
--                                 | None -> acc |> Ok
--                                 | Some parentKoncept -> 
--                                     match acc with
--                                     | Some childKoncept -> 
--                                         parentKoncept
--                                         |> ParentDimensionalKoncept 
--                                         |> add childKoncept
--                                     | None ->  parentKoncept |> Ok
--                                     |> Result.map Some
--                             Result.bind (fn2 acc) parent
--                         accKoncept |> Result.bind (fn1 parent)
--                     | DimensionalKoncept.ValueKoncept vk -> 
--                         let fn parent =
--                             match parent with
--                             | Some p -> ValueKoncept.addToDimensionalKoncept vk p
--                             | None ->  vk |> DimensionalKoncept.ValueKoncept |> Ok
--                             |> Result.map Some
--                         parent |> Result.bind fn
--                 Result.bind fmap koncept
--             map' (Ok None) koncept
--             |> Result.map (fun v -> match v with | Some vi -> Ok vi | None -> Error "Empty result from map")
--             |> Result.join
