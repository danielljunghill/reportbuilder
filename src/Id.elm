module Id exposing (..)
import UUID exposing (UUID)
import Random exposing (..)
type Id = Id UUID

type alias Seeds =
    { seed1 : Seed
    , seed2 : Seed
    , seed3 : Seed
    , seed4 : Seed
    }
create: () -> Id
create() =


    Random.step UUID.generator (Random.seed 12345)
        |> Tuple.first
        |> Id