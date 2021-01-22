module Id exposing (..)
import UUID exposing (UUID)
import Random exposing (..)
type Id = Id String

-- type alias Seeds =
--     { seed1 : Seed
--     , seed2 : Seed
--     , seed3 : Seed
--     , seed4 : Seed
--     }
create: () -> Id 
create() = Id "c61291b7-9c50-4b06-a02e-762f193fc483"

    -- Random.step UUID.generator (Random.Seed 12345)
    --     |> Tuple.first
    --     |> Id