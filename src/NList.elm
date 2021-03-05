module NList exposing (..)

import Lists exposing (..)

type alias NList a =
   {
         head: a
      ,  tail: List a
   }



create: a -> NList a
create a =
   {
         head = a
      ,  tail = []
   }

create2: a -> List a -> NList a
create2 m l =
   {
         head = m
      ,  tail = l
   }

toList: NList a -> List a
toList m = 
  [ m.head ] ++ m.tail

length: NList a -> Int
length m =
   1 + (m.tail |> List.length)
  
last:NList a -> a
last m =
   case m.tail |> Lists.rev of
       [] -> m.head
       head :: _ -> head

map: (a -> b) -> NList a -> NList b
map f m =
   {
         head = f m.head
      ,  tail = m.tail |> List.map f
   }
mapi: (Int -> a -> b) -> NList a -> NList b
mapi f m =
   let 
      innerMapi: Int -> List a -> List b
      innerMapi i im =
         case im of
            [] -> []
            head :: tail -> [ f i head ] ++ innerMapi (i + 1) tail
   in
      {
            head = f 0 m.head
         ,  tail =  innerMapi 1 m.tail         
      }

append: NList a -> List a -> NList a
append n m =
   { n | tail = n.tail ++ m}


addFirst: a -> NList a -> NList a
addFirst n m =
      {
            head = n
         ,  tail = toList m
      }

addList: NList a -> List a -> NList a
addList n m =
   { n |  tail = n.tail ++ m }



