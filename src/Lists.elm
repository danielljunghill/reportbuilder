module Lists exposing (..)

mapi: (Int -> (a -> b)) -> List a -> List b
mapi f m =
   let
      mapiInner: Int -> List a -> List b
      mapiInner i ms =
         case ms of
            [] -> []
            head :: tail -> 
               [ f i head  ] ++ (mapiInner (i + 1) tail)
   in
      mapiInner 0 m

rev: List a -> List a 
rev m =
   let
      reverseInner: List a -> List a 
      reverseInner mi =
         case mi of
            [] -> []
            head :: tail ->
               reverseInner tail ++ [ head ]
   in 
      reverseInner m

type alias NList a =
   {
         header: a
      ,  tail: List a
   }

createNList: a -> NList a
createNList a =
   {
         header = a
      ,  tail = []
   }

nListToList: NList a -> List a
nListToList m = 
  [ m.header ] ++ m.tail





