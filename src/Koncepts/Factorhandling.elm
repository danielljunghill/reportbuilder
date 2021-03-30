module Koncepts.Factorhandling exposing (fromMembers, multiply)
import Koncept.Model exposing(Factor,ValueKoncept,Memmber)
import NList exposing (..)


multiply (Factor a) (Factor b) = Factor (a * b)

membersToFactors members =
   members |> NList.map (\m -> m.factor)
membersToSingleFactor =
   membersToFactors >> multiplyAllFactors
fromMembers = membersToFactors >> multiplyAllFactors





cubeColumnSingeFactor (CubeColumn members) =
   members
   |> membersToSingleFactor

rowAndMemberFactor: ValueKoncept -> CubeColumn -> NList Factor
rowAndMemberFactor vk (CubeColumn members) =
    members 
    |> membersToFactors
    |> NList.addFirst vk.factor 

multiplyAllFactors: NList Factor -> Factor
multiplyAllFactors factors = factors |> NList.fold multiplyFactor (Factor 1) 