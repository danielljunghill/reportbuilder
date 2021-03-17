module Koncepts.CubeDimension exposing (..)
import Koncepts.CubeKoncept exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Area as Area
import Koncepts.Model exposing (..)
import Lists as Lists
import NList exposing (..)
import Tuple exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Msg exposing (..)
import Model exposing (..)

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

--- Calculate span
-- calculateSpan: Dimension -> Span -> Span
-- calculateSpan dimension (Span span)   =
--    case dimension of
--       DimensionWithDefault (_, domain) -> (span - 1) // (domain.members |> NList.length) |> Span
--       DimensionWithoutDefault (domain) -> span // (domain.members |> NList.length) |> Span
   
calculateSpan: Dimension -> Span -> Span
calculateSpan dimension (Span span)   =
   case dimension of
      DimensionWithDefault (_, domain) -> span // ((domain.members |> NList.length) + 1) |> Span
      DimensionWithoutDefault (domain) -> span // (domain.members |> NList.length) |> Span

calculateStart: Int -> Span -> Start -> Start
calculateStart ordinal (Span span) (Start start)  =
         start + span * ordinal
         |> Start

calculateArea: Dimension -> Direction -> Area -> Int -> Area
calculateArea dimension direction area ordinal =
   case direction of
      Horizontal ->
         let
            hSpan: HorizontalSpan
            hSpan = 
               area.horizontalSpan 
               |> horizontalSpanToSpan 
               |> calculateSpan dimension
               |> HorizontalSpan
            hStart: HorizontalStart
            hStart = 
               area.horizontalStart 
               |> horizontalStartToStart
               |> calculateStart ordinal  (hSpan |> horizontalSpanToSpan)
               |> HorizontalStart
            vStart: VerticalStart
            vStart = 
               area.verticalStart 
               |> verticalStartMap startIncrement 
         in
            { 
                     verticalStart = vStart
                 ,   verticalSpan = 1 |> Span |> VerticalSpan
                 ,   horizontalStart = hStart
                 ,   horizontalSpan = hSpan
                 
            }
      Vertical ->
         let
            vSpan: VerticalSpan
            vSpan = 
               area.verticalSpan
               |> verticalSpanToSpan
               |> calculateSpan dimension
               |> VerticalSpan
            vStart: VerticalStart
            vStart = 
               area.verticalStart
               |> verticalStartToStart
               |> calculateStart ordinal (vSpan |> verticalSpanToSpan)
               |> VerticalStart
            hStart: HorizontalStart 
            hStart = 
               area.horizontalStart
               |> horizontalStartMap startIncrement
         in
            { 
                     verticalStart = vStart
                 ,   verticalSpan = vSpan
                 ,   horizontalStart = hStart
                 ,   horizontalSpan = 1 |> Span |> HorizontalSpan

            }

calculateDefaultArea: Direction -> Depth -> Area -> Area
calculateDefaultArea direction (Depth depth) area =

      case direction of
         Horizontal -> 
            -- let

               {
                     verticalStart = area.verticalStart
                  ,  verticalSpan = VerticalSpan (Span depth)
                  ,  horizontalSpan = HorizontalSpan (Span 1)
                  ,  horizontalStart = 
                     area.horizontalStart  
                     |> horizontalStartMap ((spanStart (\a b -> a + b) (area.horizontalSpan |> horizontalSpanToSpan)) >> Start)
               }

         Vertical ->
               {
                     horizontalStart = area.horizontalStart
                  ,  horizontalSpan = HorizontalSpan (Span depth)
                  ,  verticalSpan = VerticalSpan  (Span 1)
                  ,  verticalStart = 
                     area.verticalStart  
                     |> verticalStartMap ((spanStart (\a b -> a + b) (area.verticalSpan |> verticalSpanToSpan)) >> Start)
               }


fromDimension: Direction -> Depth -> Area -> Dimension -> List Member -> NList DimensionHeader
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
         case defaultMemberHeader of
            Just member -> NList.append memberHeaders [ member ]
            Nothing -> memberHeaders
  

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


dimensionAsTableHeader: Direction -> Depth -> Dimension -> NList DimensionHeader -> NList TableHeader
dimensionAsTableHeader direction depth dimension headers =
   let 

      header: DimensionHeader
      header = headers.head

      columnHeaders: DimensionHeader -> NList DimensionHeader
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

   in
      header
      |> columnHeaders 
      |> NList.mapi (\ i m -> MemberHeader (NList.create2 m (mapHeaders i headers)))

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
                  {
                        verticalSpan =  0 |> Span |> VerticalSpan
                     ,  verticalStart = 0 |> Start |> VerticalStart
                     ,  horizontalSpan = totalSpan |> HorizontalSpan
                     ,  horizontalStart = 1 |> Start |> HorizontalStart
                  }

               Vertical ->
                  {
                        verticalSpan = totalSpan |> VerticalSpan
                     ,  verticalStart =  1 |> Start |> VerticalStart
                     ,  horizontalSpan = 0 |> Span |> HorizontalSpan
                     ,  horizontalStart =  0 |> Start|> HorizontalStart
                  }

   in
      case acc of
         [] -> 
            let 
               area: Area 
               area = initArea direction span
               mt: NList DimensionHeader
               mt = fromDimension direction depth area dimension []
               v1: NList TableHeader  
               v1 =  mt
                     |> NList.map (NList.create >> MemberHeader)
                     |> NList.toList
               -- v2: List TableHeader 
               -- v2 = 
               --    (second mt)
               --    |> Lists.maybeAsList
               --    |> List.map (NList.create >> TotalHeader)
            in 
               v1 
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
                  DimensionWithDefault (_,m) ->  ((NList.length m.members) + 1) * (recCalculateSpan tail)
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

type CubeColumn = CubeColumn (NList Member)
cubeColumnMembers (CubeColumn members) = members

type alias CubeColumnHeader =
   {
         isTotal: Bool
      ,  area: Area
      ,  member: Member
      ,  isSelected: Bool
   }

tableHeaderToDimensionColumnHeader: List Member -> TableHeader -> List CubeColumnHeader
tableHeaderToDimensionColumnHeader selection tableHeader   =
   let   
      createDimensionColumnHeader: List Member -> Bool -> DimensionHeader -> CubeColumnHeader
      createDimensionColumnHeader selectedMembers isTotal (DimensionHeader d) =
         let 
               selectedFactors = selectedMembers |> List.map (\v -> v.factor)
               filteredMembers: List Member
               filteredMembers =
                  d.member 
                  |> NList.toList
                  |> List.filter (\m -> (Lists.contains m.factor selectedFactors))     
         in

         {
               isTotal = isTotal
            ,  area = d.area
            ,  member = d.member.head  
            ,  isSelected = ((List.length filteredMembers) == (NList.length d.member))
         }
   in
      let
         ch: NList CubeColumnHeader
         ch =
            case tableHeader of
               MemberHeader h -> h |> NList.map (createDimensionColumnHeader selection False )
               TotalHeader h -> h |> NList.map (createDimensionColumnHeader selection True)
      in
         ch |> NList.toList


dimensionColumns: List TableHeader -> List CubeColumn
dimensionColumns headers = 
  headers 
  |> List.map (tableHeaderAsMembers >> CubeColumn)


dimensionColumnHeaders: List Member  -> List TableHeader ->  List CubeColumnHeader 
dimensionColumnHeaders selection headers  =
   headers
   |> Lists.collect (tableHeaderToDimensionColumnHeader selection)

type CubeRowOffset = CubeRowOffset Offset

cubeRowOffsetToOffset: CubeRowOffset -> Offset
cubeRowOffsetToOffset (CubeRowOffset offset) = offset
type alias CubeColumns = 
   {
         offset: CubeRowOffset
      ,  columns: List CubeColumn 
      ,  headers: List CubeColumnHeader 
   }


calculateCubeColumns:  Direction -> List Member ->  List Dimension -> CubeColumns  
calculateCubeColumns direction selection dimensions =
   let
      tableHeaders: List TableHeader
      tableHeaders = calculateTableHeaders direction dimensions
      offset : Offset
      offset =
         case direction of
            Horizontal -> 
               dimensions 
               |> List.length 
               |> Start 
               |> VerticalStart
               |> addVerticalStartToOffset Area.emptyOffset  
            Vertical -> 
               dimensions 
               |> List.length 
               |> Start 
               |> HorizontalStart 
               |> addHorizontalStartToOffset Area.emptyOffset  
   in
      {
            columns = dimensionColumns tableHeaders
         ,  headers = dimensionColumnHeaders selection tableHeaders  
         ,  offset = offset |>  CubeRowOffset
      }


--------------------------------- View -----------------------------------


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






   