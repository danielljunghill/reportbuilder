module ResultExtension exposing (..)
import Html exposing (a)


foldOne: Result err (Result err value) -> Result err value
foldOne r =
 case r of
    Ok ri -> ri
    Err err -> Err err

fromOptionWithDefault: a -> Maybe a -> Result b a
fromOptionWithDefault dv = 
   Maybe.withDefault dv  >> Result.Ok

toList: Maybe a -> List a
toList m =
   case m of
      Just v -> [ v ]
      Nothing -> []

