module Koncepts.DimensionalHeader exposing (..)
import Koncepts.Dimensionalkoncept exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.Lines exposing (..)
import Koncepts.Lines as Lines
import Lists as Lists
import NList exposing (..)
import Tuple exposing (..)
import Msg 
import Msg exposing (..)
import Html exposing (..)

type alias DimensionHeaderItem =
   {
         area: Area
      ,  member: NList Member 
   }

type DimensionHeader = DimensionHeader DimensionHeaderItem
itemFromDimensionalHeader: DimensionHeader -> DimensionHeaderItem
itemFromDimensionalHeader (DimensionHeader item) = item

createDimensionHeaderItem: Area ->  List Member -> Member -> DimensionHeaderItem
createDimensionHeaderItem a pm m  =
   {
            area = a
         ,  member = NList.create2 m pm

   }

createDimensionHeader : Area -> List Member -> Member -> DimensionHeader
createDimensionHeader a pm m  =
   createDimensionHeaderItem a pm m 
   |> DimensionHeader

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

fromDimension: Direction -> Depth -> Area -> Dimension -> List Member -> (NList DimensionHeader, Maybe DimensionHeader )
fromDimension direction depth area dimension parentMembers =
                
      let
         dms: NList DomainMember
         dms = dimensionMembers dimension
         dm: Maybe DefaultMember
         dm = memberDefault dimension
         memberHeaders: NList DimensionHeader
         memberHeaders = 
            dms
            |> NList.mapi
               (\ i (DomainMember m) ->  
                     m
                     |> createDimensionHeader (calculateArea dimension direction area i) parentMembers) 
               
         -- todo: gör om till lista som inte kan vara tom
         getLastArea: NList DimensionHeader -> Area 
         getLastArea headers = 
            headers 
            |> NList.last
            |> (\ (DimensionHeader item) -> item.area)

         defaultMemberHeader: Maybe DimensionHeader
         defaultMemberHeader =
            dm
            |> Maybe.map (\ (DefaultMember md) ->  
               md 
               |> createDimensionHeader (calculateDefaultArea direction depth (getLastArea memberHeaders)) parentMembers)
      in
         (memberHeaders, defaultMemberHeader)


-- type MemberHeader = MemberHeader DimensionHeader
-- type DefaultHeader = DefaultHeader DimensionHeader
type TableHeader  = 
   MemberHeader (NList DimensionHeader)
   | TotalHeader (NList DimensionHeader)

tableHeaderAsMembers: TableHeader -> NList Member
tableHeaderAsMembers acc =
   let 
      headers: NList DimensionHeader
      headers =
         case acc of
            MemberHeader hs -> hs
            TotalHeader hs -> hs
   in 
     headers
     |> (\ h -> h.head)
     |> (\ (DimensionHeader item)-> item.member)


-- accumulatedHeaders: TableHeader -> List DimensionHeader
-- accumulatedHeaders acc =
--    case acc of
--       MemberHeader headers -> headers |> NList.toList
--       TotalHeader headers -> headers |> NList.toList
      


dimensionAsTableHeader: Direction -> Depth -> Dimension -> NList DimensionHeader -> NList TableHeader
dimensionAsTableHeader direction depth dimension headers =
   let 

      header: DimensionHeader
      header = headers.head

      columnHeaders: DimensionHeader -> (NList DimensionHeader, Maybe DimensionHeader)
      columnHeaders (DimensionHeader item)  = 
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

      result: (NList DimensionHeader, Maybe DimensionHeader)
      result = columnHeaders header
      v1: NList TableHeader  
      v1 =  (first result)
            |> NList.mapi (\ i m -> MemberHeader (NList.create2 m (mapHeaders i headers))) 
      v2: List TableHeader 
      v2 = 
         (second result)
         |> Lists.maybeAsList
         |> List.map (NList.create >> TotalHeader)
   in
      NList.addList v1 v2

addDimensionToTableHeader: Direction -> Depth -> Dimension -> TableHeader -> List TableHeader
addDimensionToTableHeader direction depth dimension acc =
   let
      result: NList TableHeader
      result =
         case acc of
            TotalHeader _ -> NList.create acc
            MemberHeader dimensionHeaders -> dimensionAsTableHeader direction depth dimension dimensionHeaders
   in
      result |> NList.toList

addDimensionToTableHeaders: Direction -> Depth -> Span -> Dimension -> List TableHeader -> List TableHeader
addDimensionToTableHeaders direction depth span dimension acc =
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
               mt: (NList DimensionHeader, Maybe DimensionHeader)
               mt = fromDimension direction depth area dimension []
               v1: NList TableHeader  
               v1 =  (first mt)
                     |> NList.map (NList.create >> MemberHeader)
               v2: List TableHeader 
               v2 = 
                  (second mt)
                  |> Lists.maybeAsList
                  |> List.map (NList.create >> TotalHeader)
            in
               NList.addList v1 v2
               |> NList.toList
         _ -> acc |> Lists.collect (addDimensionToTableHeader direction depth dimension)

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
                  

calculateTableHeaders: Direction -> (List Dimension) -> List TableHeader
calculateTableHeaders direction dimensions =
   let
      calculateNextDepth: Depth -> Depth
      calculateNextDepth (Depth d) = (d - 1) |> Depth
      recFold: Span -> Depth -> List TableHeader -> List Dimension ->  List TableHeader 
      recFold span depth acc dims  =
         case dims of
            [] -> acc
            head :: tail ->   
               let
                  nextDepth: Depth 
                  nextDepth = calculateNextDepth depth
                  state: List TableHeader 
                  state = addDimensionToTableHeaders direction depth span head acc 
               in
                  recFold span nextDepth state tail 
   in
      let
         totalSpanForDimensions : Span
         totalSpanForDimensions = calculateSpanForDimensions dimensions
      in
         dimensions
         |> recFold totalSpanForDimensions (dimensions |> List.length |> Depth) []  

type DimensionColumn = DimensionColumn (NList Member)

type  DimensionColumns = DimensionColumns (List DimensionColumn)

type alias DimensionColumnHeader =
   {
         isTotal: Bool
      ,  area: Area
      ,  members: Member
   }

tableHeaderToDimensionColumnHeader: TableHeader -> List DimensionColumnHeader
tableHeaderToDimensionColumnHeader tableHeader =
   let   
      createDimensionColumnHeader: Bool -> DimensionHeader -> DimensionColumnHeader
      createDimensionColumnHeader isTotal (DimensionHeader d) =
         {
               isTotal = isTotal
            ,  area = d.area
            ,  members = d.member.head  
         }
   in
      let
         ch: NList DimensionColumnHeader
         ch =
            case tableHeader of
               MemberHeader h -> h |> NList.map (createDimensionColumnHeader False)
               TotalHeader h -> h |> NList.map (createDimensionColumnHeader True)
      in
   
         ch |> NList.toList


dimensionColumns: List TableHeader -> DimensionColumns
dimensionColumns headers = 
  headers 
  |> List.map (tableHeaderAsMembers >> DimensionColumn)
  |> DimensionColumns


dimensionColumnHeaders: List TableHeader -> List DimensionColumnHeader 
dimensionColumnHeaders headers =
   headers
   |> Lists.collect tableHeaderToDimensionColumnHeader



-- tableHeaderView: List TableColumn -> Html Msg
-- tableHeaderView columns = 
--    let -> 
--       tableColumAsHtml: TableColumns -> List (Html Msg)
--       tableColumnAsHtml c =
--          c.    

-- let calculateColumnsAndHeaders: Direction -> List Dimension -> (TableColumns, 

   
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

-- fromDimension: Area.Direction -> Area.Depth -> Dimensional.Dimension -> (DimensionHeader List, Maybe DimensionHeader)
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






   