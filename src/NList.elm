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


addLast:  a -> NList a -> NList a
addLast m n  = 
   { n | tail = n.tail ++ [ m ] }

fold f state alist =
   let
      recFold s l =
         case l of
         [] ->  s
         head :: tail ->
            let
               newState = f s head 
            in
               recFold newState tail
   in 
      recFold state (alist |> toList)

contains: a -> NList a -> Bool
contains v m =
   if m.head == v then
      True
   else
       m.tail 
       |> Lists.contains v



cross: List (NList a) -> (b -> NList a) -> List b -> List (NList a)
cross state f m =
   let 
      expandOne: List (NList a) -> NList a -> List (NList a) 
      expandOne l1 l2 =  
         l1 
         |> Lists.fold (\foldState a -> foldState ++ ((l2 |> map (\b -> addFirst b a )) |> toList)) []
   in
      case m of
         [] -> state
         head :: tail ->
               let
                  nextState =
                     if List.isEmpty state then
                        head
                        |> f
                        |> map (\a -> create a)
                        |> toList
                     else
                        head
                        |> f
                        |> expandOne state 
               in
                  cross nextState f tail






