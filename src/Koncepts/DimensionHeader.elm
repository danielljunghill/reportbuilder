module Koncepts.DimensionHeader exposing (..)
import Koncepts.Dimensionalkoncept exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.Lines exposing (..)
import Koncepts.Lines as Lines
import Lists as Lists
import NList exposing (..)
import Tuple exposing (..)

type alias DimensionalHeaderItem =
   {
         area: Area
      ,  member: NList Member 
   }

type DimensionalHeader = DimensionalHeader DimensionalHeaderItem
itemFromDimensionalHeader: DimensionalHeader -> DimensionalHeaderItem
itemFromDimensionalHeader (DimensionalHeader item) = item

createDimensionalHeaderItem: Area ->  List Member -> Member -> DimensionalHeaderItem
createDimensionalHeaderItem a pm m  =
   {
            area = a
         ,  member = NList.create2 m pm

   }

createDimensionalHeader : Area -> List Member -> Member -> DimensionalHeader
createDimensionalHeader a pm m  =
   createDimensionalHeaderItem a pm m 
   |> DimensionalHeader

calculateSpan: Dimension -> Span -> Span
calculateSpan dimension (Span span)   =
   case dimension of
      DimensionWithDefault (_, domain) -> (span - 1) // (domain.members |> NList.length) |> Span
      DimensionWithoutDefault (domain) -> span // (domain.members |> NList.length) |> Span
   

calculateStart: Int -> Start -> Span -> Start
calculateStart ordinal (Start start) (Span span)  =
         start + span * ordinal
         |> Start

calculateArea: Dimension -> Direction -> Area -> Int -> Area
calculateArea dimension direction area ordinal =
   case direction of
      Horizontal ->
         let
            hSpan: Span
            hSpan = Lines.horizontalSpan area.horizontalLine |> calculateSpan dimension
            hStart: Start
            hStart = calculateStart ordinal (Lines.horizontalStart area.horizontalLine) hSpan
            vStart: Start 
            vStart = Lines.startAdd (Lines.verticalStart area.verticalLine) (Start 1)    
         in
            { 
                  horizontalLine = HorizontalLine (createLine hStart hSpan)  -- HorizontalLine    { span = horizontalSpan; start = horizontalstart} 
               ,  verticalLine = VerticalLine (createLine vStart (Span 1))  --VerticalLine { span =  ; start = verticalStart}
            }
      Vertical ->
         let
            vSpan: Span
            vSpan = Lines.verticalSpan area.verticalLine |> calculateSpan dimension
            vStart: Start
            vStart = calculateStart ordinal (Lines.verticalStart area.verticalLine) vSpan
            hStart: Start 
            hStart = Lines.startAdd (Lines.horizontalStart area.horizontalLine) (Start 1)
         in
            { 
                  horizontalLine = HorizontalLine (createLine hStart (Span 1)) -- { Span = Span 1; Start = horizontalStart}
               ,  verticalLine = VerticalLine (createLine vStart vSpan)--{ Span = verticalSpan ; Start = verticalStart }
            }

calculateDefaultArea: Direction -> Depth -> Area -> Area
calculateDefaultArea direction (Depth depth) area =
   let
      vl: VerticalLine
      vl = area.verticalLine
      hl: HorizontalLine
      hl = area.horizontalLine
   in
      case direction of
         Horizontal -> 
            let
               newVl: VerticalLine 
               newVl =
                  vl
                  |> verticalSetSpan (Span depth)
               newHl: HorizontalLine
               newHl =
                  hl
                  |> horizontalIncrementStartWithSpan 
                  |> horizontalSetSpan (Span 1)
            in
               { 
                     verticalLine = newVl
                  ,  horizontalLine = newHl
               }
         Vertical ->
            let
               newHl: HorizontalLine 
               newHl =
                  hl
                  |> horizontalSetSpan (Span depth)
               newVl: VerticalLine
               newVl =
                  vl
                  |> verticalIncrementStartWithSpan 
                  |> verticalSetSpan (Span 1)
            in
               { 
                     verticalLine = newVl
                  ,  horizontalLine = newHl
               }

fromDimension: Direction -> Depth -> Area -> Dimension -> List Member -> (NList DimensionalHeader, Maybe DimensionalHeader )
fromDimension direction depth area dimension parentMembers =
                
      let
         dms: NList DomainMember
         dms = dimensionMembers dimension
         dm: Maybe DefaultMember
         dm = memberDefault dimension
         memberHeaders: NList DimensionalHeader
         memberHeaders = 
            dms
            |> NList.mapi
               (\ i (DomainMember m) ->  
                     m
                     |> createDimensionalHeader (calculateArea dimension direction area i) parentMembers) 
               
         -- todo: gör om till lista som inte kan vara tom
         getLastArea: NList DimensionalHeader -> Area 
         getLastArea headers = 
            headers 
            |> NList.last
            |> (\ (DimensionalHeader item) -> item.area)

         defaultMemberHeader: Maybe DimensionalHeader
         defaultMemberHeader =
            dm
            |> Maybe.map (\ (DefaultMember md) ->  
               md 
               |> createDimensionalHeader (calculateDefaultArea direction depth (getLastArea memberHeaders)) parentMembers)
      in
         (memberHeaders, defaultMemberHeader)


-- type MemberHeader = MemberHeader DimensionalHeader
-- type DefaultHeader = DefaultHeader DimensionalHeader
type AccumulatedHeader = 
   MemberHeader (NList DimensionalHeader)
   | TotalHeader (NList DimensionalHeader)

accumulatedMembers: AccumulatedHeader -> NList Member
accumulatedMembers acc =
   let 
      headers: NList DimensionalHeader
      headers =
         case acc of
            MemberHeader hs -> hs
            TotalHeader hs -> hs
   in 
     headers
     |> (\ h -> h.head)
     |> (\ (DimensionalHeader item)-> item.member)

accumulatedHeaders: AccumulatedHeader -> List DimensionalHeader
accumulatedHeaders acc =
   case acc of
      MemberHeader headers -> headers |> NList.toList
      TotalHeader headers -> headers |> NList.toList
      


addDimensionToMemberHeader: Direction -> Depth -> Dimension -> NList DimensionalHeader -> NList AccumulatedHeader
addDimensionToMemberHeader direction depth dimension headers =
   let 

      header: DimensionalHeader
      header = headers.head

      columnHeaders: DimensionalHeader -> (NList DimensionalHeader, Maybe DimensionalHeader)
      columnHeaders (DimensionalHeader item)  = 
         let
            area: Area 
            area = item.area
            members: List Member
            members = item.member |> NList.toList
         in 
            fromDimension direction depth area dimension members

      mapHeaders: Int -> NList a -> List a
      mapHeaders i hs =
         if i == 0 then
            hs |> NList.toList
         else 
            []

      result: (NList DimensionalHeader, Maybe DimensionalHeader)
      result = columnHeaders header
      v1: NList AccumulatedHeader  
      v1 =  (first result)
            |> NList.mapi (\ i m -> MemberHeader (NList.create2 m (mapHeaders i headers))) 
      v2: List AccumulatedHeader 
      v2 = 
         (second result)
         |> Lists.maybeAsList
         |> List.map (NList.create >> TotalHeader)
   in
      NList.addList v1 v2

addDimensionToAccumulatedHeader: Direction -> Depth -> Dimension -> AccumulatedHeader -> List AccumulatedHeader
addDimensionToAccumulatedHeader direction depth dimension acc =
   let
      result: NList AccumulatedHeader
      result =
         case acc of
            TotalHeader _ -> NList.create acc
            MemberHeader dimensionHeaders -> addDimensionToMemberHeader direction depth dimension dimensionHeaders
   in
      result |> NList.toList

addDimensionToAccumulatedHeaders: Direction -> Depth -> Span -> Dimension -> List AccumulatedHeader -> List AccumulatedHeader
addDimensionToAccumulatedHeaders direction depth span dimension acc =
   let 
      initArea: Direction -> Span -> Area
      initArea d totalSpan =
            case d of
               Horizontal ->
                  { verticalLine = VerticalLine { span = Span 0, start = Start 0},  horizontalLine = HorizontalLine {span = totalSpan, start = Start 1}}
               Vertical ->
                  { verticalLine = VerticalLine  { span = totalSpan, start = Start 1}, horizontalLine = HorizontalLine {span = Span 0, start = Start 0}}
   in
      case acc of
         [] -> 
            let 
               area: Area 
               area = initArea direction span
               mt: (NList DimensionalHeader, Maybe DimensionalHeader)
               mt = fromDimension direction depth area dimension []
               v1: NList AccumulatedHeader  
               v1 =  (first mt)
                     |> NList.map (NList.create >> MemberHeader)
               v2: List AccumulatedHeader 
               v2 = 
                  (second mt)
                  |> Lists.maybeAsList
                  |> List.map (NList.create >> TotalHeader)
            in
               NList.addList v1 v2
               |> NList.toList
         _ -> acc |> Lists.collect (addDimensionToAccumulatedHeader direction depth dimension)

calculateSpanForDimensions: List Dimension -> Span
calculateSpanForDimensions dimensions =
   let
      recCalculateSpan: List Dimension -> Int
      recCalculateSpan d =
         case d of
            [] -> 1
            head :: tail ->
               case head of
                  DimensionWithDefault (_,m) ->  (NList.length m.members) * (recCalculateSpan tail) + 1 
                  DimensionWithoutDefault (m)-> (NList.length m.members) * (recCalculateSpan tail)
   in
      dimensions
      |> recCalculateSpan 
      |> Span
                  

calculateHeadersForDimensions: Direction -> (List Dimension) -> List AccumulatedHeader
calculateHeadersForDimensions direction dimensions =
   let
      calculateNextDepth: Depth -> Depth
      calculateNextDepth (Depth d) = (d - 1) |> Depth
      recFold: Span -> Depth -> List AccumulatedHeader -> List Dimension ->  List AccumulatedHeader 
      recFold span depth acc dims  =
         case dims of
            [] -> acc
            head :: tail ->   
               let
                  nextDepth: Depth 
                  nextDepth = calculateNextDepth depth
                  state: List AccumulatedHeader 
                  state = addDimensionToAccumulatedHeaders direction depth span head acc 
               in
                  recFold span nextDepth state tail 
   in
      let
         totalSpanForDimensions : Span
         totalSpanForDimensions = calculateSpanForDimensions dimensions
      in
         dimensions
         |> recFold totalSpanForDimensions (dimensions |> List.length |> Depth) []  




   
   --      let (SimpleHeader (Header s)) = headers.Head
   -- let simples,total = Header.fromDimension direction depth s.Area dimension
   -- // match header with
   -- let notWritableHeaders = headers |> List.map (fun (SimpleHeader (Header item)) -> { item with Write = false } |> Header |> SimpleHeader)
   -- let mapHeaders i headers= 
   --    if i = 0 then 
   --       headers
   --    else notWritableHeaders 
      
   -- // | Simple s -> 
   -- let v1 = simples |> List.mapi (fun i simple -> Simple (SimpleHeader simple :: (mapHeaders i headers ))) 
   -- let v2 = total |> Option.toList |> List.map (fun t -> Total (TotalHeader t,notWritableHeaders))
   -- v1 @ v2

-- fromDimension: Area.Direction -> Area.Depth -> Dimensional.Dimension -> (DimensionalHeader List, Maybe DimensionalHeader)
-- fromDimension direction depth area dimension =
--    let
--       members: List DomainMember 
--       members = Dimension.members dimension
--       defaultMember: Maybe DefaultMember
--       defaultMember = Dimension.defaultMember dimension

--       calcSpan: Maybe DefaultMember ->  Span -> Span
--       calcSpan span defaultMember =
--          case defaultMember of
--             Just _ -> spanAdd (-1) span
--             Nothing -> span
--          / members.length

--       calcStart: Int -> Start -> Span -> Start
--       calcStart ordinal (Start start) (Span span) =
--          start + span * ordinal
--          |> Start






   