module Koncepts.Factorhandling exposing (..)
import Koncepts.Model exposing(Factor(..),ValueKoncept,Member)
import NList exposing (..)
import Model exposing (..)
import Koncepts.CubeModel exposing (CubeColumn(..))

multiply1 (Factor a) (Factor b) = Factor (a * b)

-- membersToFactors members =
--    members |> NList.map (\m -> m.factor)
-- membersToSingleFactor =
--    membersToFactors >> multiplyAllFactors
-- fromMembers = membersToFactors >> multiplyAllFactors

-- cubeColumnSingeFactor (CubeColumn members) =
--    members
--    |> membersToSingleFactor

-- rowAndMemberFactor: ValueKoncept -> CubeColumn -> NList Factor
-- rowAndMemberFactor vk (CubeColumn members) =
--     members 
--     |> membersToFactors
--     |> NList.addFirst vk.factor 

-- multiplyAllFactors: NList Factor -> Factor
-- multiplyAllFactors factors = factors |> NList.fold multiply (Factor 1) 


