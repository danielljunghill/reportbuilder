module Koncepts.Mock exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.Koncept as Koncept
import Koncepts.Hypercube as HyperCube 
import Koncepts.Koncept exposing (KonceptAction)
import Koncepts.CubeKoncept as CubeKoncept
import Array exposing (foldr)
import Koncepts.Area

import Json.Decode exposing (string)
import NList exposing (..)
import NList as NList
import Prime exposing (..)
import Prime 
import Koncepts.CubeDimension
import Koncepts.Hypercube exposing (..)
import Koncepts.CubeRowHeader exposing (..)
import Koncepts.CubeModel exposing(..)
import Koncepts.CubeRow exposing(..)
import Koncepts.CubeView2

firstPrime:Prime
firstPrime =  Prime.init

head: Result String Koncept
head = 
    "IORP2 nationell"  
    |> Koncept.createAbstract (AbstractFactor 1)
    |> Koncept.ParentKoncept  
    |> Koncept.add ("Intäkter" |> Koncept.createAbstract (AbstractFactor 2) )

addAbstract: String -> Result String Koncept -> Result String Koncept
addAbstract name koncept =
    koncept
    |> Result.map Koncept.ParentKoncept 
    |> Result.andThen (name |> Koncept.createAbstract (AbstractFactor 3) |> Koncept.add)

regions: PrimeResult HyperDimension
regions = 
   firstPrime
   |> createDimensionWithDefault (DomainName "Produkt") (NList.create2 "Personbil" ["Lastbil"]) 
   |> mapPrimeResult Closed

years: PrimeResult HyperDimension
years = 
   regions.prime
   |> createDimensionWithDefault (DomainName "Artal") (NList.create2 "2020" ["2021"]) 
   |> mapPrimeResult Closed

quarters: PrimeResult HyperDimension
quarters = 
   years.prime
   |> createDimensionWithDefault (DomainName "Kvartal") (NList.create2 "kv1"  ["kv2"])
   |> mapPrimeResult Closed

addCube: Koncept -> Result String (Maybe Koncept)
addCube  koncept  =  
   let 
      hyperCube: HyperCube
      hyperCube = 
         quarters.result
         |> HyperCube.create "Kvartal och annat" 
         |> HyperCube.addDimension years.result 
         |> HyperCube.addDimension regions.result 
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

dimKonceptBikes: PrimeResult DimensionalKoncept
dimKonceptBikes =
   quarters.prime

   |> CubeKoncept.createValue "Försäljning cyklar"
dimKonceptSubsidies: PrimeResult DimensionalKoncept
dimKonceptSubsidies =
   dimKonceptBikes.prime
   |> CubeKoncept.createValue "Bidrag"


dimKonceptLon: PrimeResult DimensionalKoncept
dimKonceptLon =
   dimKonceptSubsidies.prime
   |> CubeKoncept.createValue "Löner"

dimKonceptLonMaterial: PrimeResult DimensionalKoncept
dimKonceptLonMaterial =
   dimKonceptLon.prime
   |> CubeKoncept.createValue "Ingående Material"


addDimensionalKoncept:Koncept -> Result String (Maybe Koncept) 
addDimensionalKoncept  =
   let
      dims : List DimensionalKoncept 
      dims = [ CubeKoncept.createAbstract (AbstractFactor 4)  [ dimKonceptBikes.result , dimKonceptSubsidies.result ] "Intäkter" ,  CubeKoncept.createAbstract (AbstractFactor 5) [ dimKonceptLon.result , dimKonceptLonMaterial.result ] "Kostnader" ]
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
   |> Result.andThen (Koncept.fold addCube)
   |> Result.andThen (Koncept.fold addDimensionalKoncept)
   -- |> addValue "Ett nytt värde 4"




 

-- let added5 = Koncept.map addDimensionalKoncept added4

-- let addValue  =
--     let f (ak:AbstractKoncept) koncepts =
--        if ak.Name = AbstractKonceptName "Head abstract1" then
--             (ak, koncepts @ [ "dagen d" |> ValueKoncept.create |> Koncept.ValueKoncept]) |> Koncept.AbstractKoncept |> MapKonceptAction.NewValue
--        else
--             MapKonceptAction.Ignore
--        |> Ok
--     mapAbstractKoncept f  