module Lists exposing (..)

mapi: (Int -> (a -> b)) -> List a -> List b
mapi f m =
   let
      recMapi: Int -> List a -> List b
      recMapi i ms =
         case ms of
            [] -> []
            head :: tail -> 
               [ f i head  ] ++ (recMapi (i + 1) tail)
   in
      recMapi 0 m

rev: List a -> List a 
rev m =
   let
      recRevers: List a -> List a 
      recRevers mi =
         case mi of
            [] -> []
            head :: tail ->
               recRevers tail ++ [ head ]
   in 
      recRevers m


maybeAsList: Maybe a -> List a
maybeAsList a =
   case a of
      Just v -> [ v ]
      Nothing -> []


collect: (a -> List b) -> List a -> List b
collect f m =
   let 
      recCollect: List a -> List b
      recCollect rm =
         case rm of
         [] -> []
         head :: tail -> 
            (f head) ++ recCollect tail
   in
      recCollect m




maxBy: (a -> a -> Bool)  -> List a -> Maybe a
maxBy f m =
   let
      recMax: Maybe a -> List a  -> Maybe a
      recMax state rm =
         case rm of
            [] -> state
            head :: tail -> 
               case state of
                  Just v -> 
                     if f v head then
                        recMax state tail
                     else recMax (Just head) tail
                  Nothing ->
                     recMax (Just head) tail
   in
     m |> recMax Nothing

maxInt: List Int  -> Maybe Int
maxInt = maxBy (\a b -> a > b)             


