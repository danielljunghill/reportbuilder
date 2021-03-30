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


contains: a -> List a -> Bool
contains n l =
   let
      recContains m =
         case m of
         [] -> False
         head :: tail ->
            if head == n then True 
            else recContains tail
   in
      recContains l


foldi: (Int -> state -> a -> state) -> state -> List a ->  state
foldi f s m =
   let
      recFoldi i state l =
         case l of
         [] ->  state
         head :: tail ->
            let
               newState = f i state head 
            in
               recFoldi (i + 1) newState tail
   in 
      recFoldi 0 s m

fold: (state -> a -> state) -> state -> List a ->  state
fold f s m =
   let
      recFold state l =
         case l of
         [] ->  state
         head :: tail ->
            let
               newState = f state head 
            in
               recFold newState tail
   in 
      recFold s m




cross: List (List a) -> (b -> List a) -> List b -> List (List a)
cross state f m =
   let 
      expandOne: List (List a) -> List a -> List (List a) 
      expandOne l1 l2 =  
         l1 
         |> fold (\foldState a -> foldState ++ (l2 |> List.map (\b -> [ b ] ++ a))) []
   in
      case m of
         [] -> state
         head :: tail ->
               let
                  nextState =
                     if List.isEmpty state then
                        head
                        |> f
                        |> List.map (\a -> [ a ])
                     else
                        head
                        |> f
                        |> expandOne state 
               in
                  cross nextState f tail
