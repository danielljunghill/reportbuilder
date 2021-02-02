module ResultExtension exposing (..)


foldOne: Result err (Result err value) -> Result err value
foldOne r =
 case r of
    Ok ri -> ri
    Err err -> Err err

fromOptionWithDefault: a -> Maybe a -> Result b a
fromOptionWithDefault dv = 
   Maybe.withDefault dv  >> Result.Ok

