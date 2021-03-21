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
      ,  members: NList Member
      ,  isTotal: Bool
   }

type DimensionHeader = DimensionHeader DimensionHeaderItem
itemFromDimensionalHeader: DimensionHeader -> DimensionHeaderItem
itemFromDimensionalHeader (DimensionHeader item) = item

createDimensionHeaderItem: Bool -> Area ->  List Member -> Member -> DimensionHeaderItem
createDimensionHeaderItem isTotal area members m  =
   {
            area = area
         ,  members = NList.create2 m members
         , isTotal = isTotal

   }

createDimensionHeader : Bool -> Area -> List Member -> Member -> DimensionHeader
createDimensionHeader isTotal area members m  =
   createDimensionHeaderItem isTotal area members m 
   |> DimensionHeader


--calculate span for dimensions  
-- Total span di
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


fromDimension: Direction -> Area -> Dimension -> List Member -> NList DimensionHeader
fromDimension direction area dimension parentMembers =              
      let
         countMembers = NList.length (dimension |> dimensionMembers)

         defaultMembers: List DimensionHeader
         defaultMembers = 
            dimension
            |> memberDefault 
            |> Maybe.map (\(DefaultMember m) -> createDimensionHeader True (calculateArea dimension direction area countMembers) parentMembers m)
            |> Lists.maybeAsList
            
         memberHeaders: NList DimensionHeader
         memberHeaders = 
            dimension
            |> dimensionMembers 
            |> NList.map (\ (DomainMember m) -> m)
            |> NList.mapi (\ i m ->  m |> createDimensionHeader False (calculateArea dimension direction area i) parentMembers)                  
      in
         NList.append memberHeaders defaultMembers

type MemberHeader  =   
   MemberHeader (NList DimensionHeader)

tableHeaderAsMembers: MemberHeader -> NList Member
tableHeaderAsMembers (MemberHeader dimensionHeaders) =
     dimensionHeaders
     |> (\ h -> h.head)
     |> (\ (DimensionHeader item)-> item.members)


dimensionAsTableHeader: Direction -> Dimension -> NList DimensionHeader -> NList MemberHeader
dimensionAsTableHeader direction dimension dimensionHeaders =
   let 
      -- take last dimensionheader from
      -- parents (dimensionHeaders) since it holds all previous Members
      -- for tableheader
      header: DimensionHeader
      header = dimensionHeaders.head
      -- create dimensionheader from dimension
      -- with area and members from parent header
      createDimensionHeaders: DimensionHeader -> NList DimensionHeader
      createDimensionHeaders (DimensionHeader item)  = 
         let
            area: Area 
            area = item.area
            members: List Member
            members = item.members |> NList.toList
         in 
            fromDimension direction area dimension members
      -- only fitst
      -- mapHeaders: Int -> NList a -> List a
      -- mapHeaders i hs =
      --    if i == 0 then hs |> NList.toList
      --    else []

   in
      header
      |> createDimensionHeaders 
      |> NList.map (\ m -> MemberHeader (NList.create2 m (dimensionHeaders |> NList.toList)))
 

addDimensionToTableHeader: Direction -> Dimension -> MemberHeader -> List MemberHeader
addDimensionToTableHeader direction dimension (MemberHeader dimensionHeaders) =
   dimensionAsTableHeader direction dimension dimensionHeaders
   |> NList.toList


addDimensionToTableHeaders: Direction  -> Span -> Dimension -> List MemberHeader -> List MemberHeader
addDimensionToTableHeaders direction span dimension acc =
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
               initArea direction span
               |> (\area -> fromDimension direction area dimension [])
               |> NList.map (NList.create >> MemberHeader)
               |> NList.toList
        
         _ -> acc |> Lists.collect (addDimensionToTableHeader direction dimension)

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
                  

calculateTableHeaders: Direction -> (List Dimension) -> List MemberHeader
calculateTableHeaders direction dimensions =
   let
      recFold: Span -> List MemberHeader -> List Dimension ->  List MemberHeader 
      recFold span  acc dims  =
         case dims of
            [] -> acc
            head :: tail ->   
               let
                  state: List MemberHeader 
                  state = addDimensionToTableHeaders direction span head acc 
               in
                  recFold span state tail 
   in
      let
         totalSpanForDimensions : Span
         totalSpanForDimensions = calculateSpanForDimensions dimensions
      in
         dimensions
         |> recFold totalSpanForDimensions []  

type CubeColumn = CubeColumn (NList Member)
cubeColumnMembers (CubeColumn members) = members

type alias CubeColumnHeader =
   {
         isTotal: Bool
      ,  area: Area
      ,  member: Member
      ,  isSelected: Bool
   }

-- tableHeaderToDimensionColumnHeader: List Member -> MemberHeader -> List CubeColumnHeader
-- tableHeaderToDimensionColumnHeader selection (MemberHeader dimensionHeaders)   =
--    let   
--       createDimensionColumnHeader: List Member -> Bool -> DimensionHeader -> CubeColumnHeader
--       createDimensionColumnHeader selectedMembers isTotal (DimensionHeader d) =
--          let 
--                selectedFactors = selectedMembers |> List.map (\v -> v.factor)
--                filteredMembers: List Member
--                filteredMembers =
--                   d.members 
--                   |> NList.toList
--                   |> List.filter (\m -> (Lists.contains m.factor selectedFactors))     
--          in

--          {
--                isTotal = d.isTotal
--             ,  area = d.area
--             ,  member = d.members.head  
--             ,  isSelected = ((List.length filteredMembers) == (NList.length d.members))
--          }
--    in

--       dimensionHeaders
--       |> NList.map (\dm -> createDimensionColumnHeader selection True dm)
--       |> NList.toList
  
tableHeaderToDimensionColumnHeader: List Factor -> MemberHeader -> List CubeColumnHeader
tableHeaderToDimensionColumnHeader selectedFactors (MemberHeader dimensionHeaders)   =
   let   
      createDimensionColumnHeader: Bool -> DimensionHeader -> CubeColumnHeader
      createDimensionColumnHeader  isTotal (DimensionHeader d) =
         let 
               -- selectedFactors = selectedMembers |> List.map (\v -> v.factor)
               filteredMembers: List Member
               filteredMembers =
                  d.members 
                  |> NList.toList
                  |> List.filter (\m -> (Lists.contains m.factor selectedFactors))     
         in

         {
               isTotal = d.isTotal
            ,  area = d.area
            ,  member = d.members.head  
            ,  isSelected = ((List.length filteredMembers) == (NList.length d.members))
         }
   in

      dimensionHeaders
      |> NList.map (\dm -> createDimensionColumnHeader True dm)
      |> NList.toList
  
dimensionColumns: List MemberHeader -> List CubeColumn
dimensionColumns headers = 
  headers 
  |> List.map (tableHeaderAsMembers >> CubeColumn)


dimensionColumnHeaders: List Factor  -> List MemberHeader ->  List CubeColumnHeader 
dimensionColumnHeaders selection headers  =
   headers
   |> Lists.collect (tableHeaderToDimensionColumnHeader selection)

type CubeRowOffset = CubeRowOffset Offset

cubeRowOffsetToOffset: CubeRowOffset -> Offset
cubeRowOffsetToOffset (CubeRowOffset offset) = offset

----
-- CubeColumns holds offset, columns for cube and headers for cube
----
type alias CubeColumns = 
   {
         offset: CubeRowOffset
      ,  columns: List CubeColumn 
      ,  headers: List CubeColumnHeader 
   }


calculateCubeColumns:  Direction -> List Factor ->  List Dimension -> CubeColumns  
calculateCubeColumns direction selection dimensions =
   let
      tableHeaders: List MemberHeader
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


