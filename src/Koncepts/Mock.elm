module Koncepts.Mock exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.Koncept as Koncept
import Koncepts.Hypercube as HyperCube 
import Koncepts.Koncept exposing (KonceptAction)
import Koncepts.Dimensionalkoncept as DimensionalKoncept
import Array exposing (foldr)
import Koncepts.Area
import Koncepts.Lines
import Koncepts.DimensionalHeader
import Json.Decode exposing (string)
import NList exposing (..)
import NList as NList




head: Result String Koncept
head = 
    "IORP2 nationell strunt"  
    |> Koncept.createAbstract 
    |> Koncept.ParentKoncept  
    |> Koncept.add ("Intäkter" |> Koncept.createAbstract )

addValue: String -> Result String Koncept -> Result String Koncept
addValue name koncept =
    koncept
    |> Result.map Koncept.ParentKoncept 
    |> Result.andThen (name |> Koncept.createValue |> Koncept.add)

addAbstract: String -> Result String Koncept -> Result String Koncept
addAbstract name koncept =
    koncept
    |> Result.map Koncept.ParentKoncept 
    |> Result.andThen (name |> Koncept.createAbstract |> Koncept.add)

addCube: Koncept -> Result String (Maybe Koncept)
addCube koncept  =
      let
        regioner: NList String
        regioner = NList.create2 "Sverige"  ["Norge"]
        dimRegioner: HyperDimension
        dimRegioner = 
            regioner 
            |> HyperCube.domainCreate "Region" 
            |> HyperCube.createDimensionWithDefault
            |> Closed
        kvartal: NList String
        kvartal = NList.create2 "kv1"  ["kv2", "kv3","kv4"]
        hyperCube: HyperCube
        hyperCube = 
         kvartal
         |> HyperCube.domainCreate "Kvartal" 
         |> HyperCube.createDimensionWithDefault
         |> Closed
         |> HyperCube.create "Kvartal och annat tjafs"  
         |> (\cube -> HyperCube.addDimension cube dimRegioner)
         -- |> 
        
      in
         let
            f: AbstractKoncept -> List Koncept -> Result String KonceptAction
            f ak koncepts =
               if ak.name == (AbstractKonceptName "Intäkter") then
                  (ak, koncepts ++ ([ Cube (hyperCube, []) ]))
                  |> Abstract
                  |> MapValue
                  |> Ok
               else
                  Ignore |> Ok            
         in
            Koncept.mapAbstractKoncept f koncept

addDimensionalKoncept:Koncept -> Result String (Maybe Koncept) 
addDimensionalKoncept  =
   let
      dims : List DimensionalKoncept 
      dims = [ DimensionalKoncept.createValue "Intäkter" , DimensionalKoncept.createValue "Försäljning cyklar", DimensionalKoncept.createValue "Bidrag" ]
   in
      let
          f: HyperCube -> List DimensionalKoncept -> Result String KonceptAction
          f hc dimensions =
               if hc.name == HyperCubeName "Kvartal och annat" then
                  (hc,dimensions ++ dims) 
                  |> Cube 
                  |> MapValue
                  |> Ok
               else
                  Ignore 
                  |> Ok
      
      in
         Koncept.mapCube f
      
mockKoncept: Result String Koncept
mockKoncept =
  
   head
   |> Koncept.fold addCube
   |> Koncept.fold addDimensionalKoncept
   |> addValue "Ett nytt värde"




 

-- let added5 = Koncept.map addDimensionalKoncept added4

-- let addValue  =
--     let f (ak:AbstractKoncept) koncepts =
--        if ak.Name = AbstractKonceptName "Head abstract1" then
--             (ak, koncepts @ [ "dagen d" |> ValueKoncept.create |> Koncept.ValueKoncept]) |> Koncept.AbstractKoncept |> MapKonceptAction.NewValue
--        else
--             MapKonceptAction.Ignore
--        |> Ok
--     mapAbstractKoncept f  