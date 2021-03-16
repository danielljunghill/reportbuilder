module Prime exposing (Prime, generatePrime, PrimeNumberGenerator(..), init, PrimeResult, mapPrimeResult, createPrimeResult)
import NList exposing (..)
import Basics.Extra exposing (..)

type alias Prime = { numbers: NList Int }

type PrimeNumberGenerator = PrimeNumberGenerator (Prime -> Prime)

init: Prime
init =
    {
        numbers = NList.create 2
    }


bFactorOfA: Int -> Int -> Bool 
bFactorOfA a b = (b * (a // b)) == a
  




tryGetLastNumber: List Int -> Maybe Int
tryGetLastNumber numbers =
    case numbers of
        [] -> Nothing
        head :: tail -> Just head

tryNumberAsPrime: Int -> List Int -> Maybe Int
tryNumberAsPrime n numbers =
    case numbers of
        [] -> n |> Just
        head :: tail ->
            if bFactorOfA n head then Nothing
            else tryNumberAsPrime n tail

generatePrime: Prime -> Prime
generatePrime prime =
    let
        numbers: List Int
        numbers = prime.numbers |> NList.toList
    in
        case tryGetLastNumber numbers of
            Nothing -> { numbers = NList.create 2}
            Just nr ->
                let 
                    recGetNextPrime: Int -> Prime
                    recGetNextPrime number =
                        case tryNumberAsPrime number numbers of
                            Just nextNr -> { prime | numbers = NList.addFirst nextNr prime.numbers}
                            Nothing -> recGetNextPrime (number + 1)
                in 
                    recGetNextPrime (nr + 1)
    

type alias PrimeResult a = 
    {
            result: a
        ,   prime: Prime
    }

createPrimeResult: a -> Prime -> PrimeResult a
createPrimeResult m prime =
    {
            result = m
        ,   prime = prime 
    }

mapPrimeResult: (a -> b) -> PrimeResult a -> PrimeResult b
mapPrimeResult f m =
    let
        result: b
        result = f m.result
    in 
       {
                prime = m.prime
           ,    result = result    
       }

---

---