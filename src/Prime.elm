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



tryGetLastNumber: List Int -> Maybe Int
tryGetLastNumber numbers =
    case numbers of
        [] -> Nothing
        head :: tail -> Just head

trytNumberAsPrime: Int -> List Int -> Maybe Int
trytNumberAsPrime n numbers =
        case numbers of
            [] ->   
                n
                |> Just 
            head :: tail ->
                let
                    isFactorIn: Int -> Int -> Bool
                    isFactorIn taljare namnare   =
                        let
                            test = Debug.log "namnare" namnare
                        in
                            case safeModBy taljare namnare  of
                                Just v -> v == 0
                                Nothing -> True -- TODO: This should be changed into 
                in
                    if  isFactorIn n head   then Nothing
                    else trytNumberAsPrime n tail

generatePrime: Prime -> Prime
generatePrime prime  =
    let 
        numbers: List Int
        numbers = 
            prime.numbers
            |> NList.toList
    in 
        case tryGetLastNumber numbers of
            Nothing ->  { prime | numbers = NList.create 2}
            Just nr -> 
                let 
                    recGetNextPrimeNumber: Int -> Prime
                    recGetNextPrimeNumber nnr  =
                        case trytNumberAsPrime nnr numbers of
                            Just pn -> { prime | numbers = NList.addFirst pn prime.numbers }
                            Nothing -> recGetNextPrimeNumber (nnr + 1) 
                in
                    recGetNextPrimeNumber (nr + 1)

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