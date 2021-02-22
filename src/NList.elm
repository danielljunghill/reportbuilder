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
  
getLast:NList a -> a
getLast m =
   case m.tail |> Lists.rev of
       [] -> m.head
       head :: _ -> head

map: (a -> b) -> NList a -> NList b
map f m =
   {
         head = f m.head
      ,  tail = m.tail |> List.map f
   }

append: NList a -> List a -> NList a
append n m =
   { n | tail = n.tail ++ m}






