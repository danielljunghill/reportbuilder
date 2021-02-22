module Koncepts.DimensionHeader exposing (..)
import Koncepts.Dimensionalkoncept exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.Lines exposing (..)
import Koncepts.Lines as Lines
import Lists as Lists
import NList exposing (..)

type alias DimensionalHeaderItem =
   {
         area: Area
      ,  member: Member
      ,  write: Bool
   }

type DimensionalHeader = DimensionalHeader DimensionalHeaderItem

createDimensionalHeaderItem: Area -> Member -> DimensionalHeaderItem
createDimensionalHeaderItem a m =
   {
            area = a
         ,  member = m
         ,  write = True
   }

createDimensionalHeader : Area -> Member  -> DimensionalHeader
createDimensionalHeader a m =
   createDimensionalHeaderItem a m 
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

fromDimension: Direction -> Depth -> Area -> Dimension -> (List DimensionalHeader, Maybe DimensionalHeader )
fromDimension direction depth area dimension =
                
      let
         dms: NList DomainMember
         dms = dimensionMembers dimension
         dm: Maybe DefaultMember
         dm = memberDefault dimension
         memberHeaders: List DimensionalHeader
         memberHeaders = 
            dms
            |> NList.toList
            |> Lists.mapi 
               (\ i (DomainMember m) ->  
                     m
                     |> createDimensionalHeader (calculateArea dimension direction area i))
               
         -- todo: gör om till lista som inte kan vara tom
         getLastArea: List DimensionalHeader -> Area 
         getLastArea headers = 
            headers 
            |> Lists.rev 
            |> List.head 
            |> Maybe.map (\(DimensionalHeader item) -> item.area)
            |> Maybe.withDefault emptyArea

         defaultMemberHeader: Maybe DimensionalHeader
         defaultMemberHeader =
            dm
            |> Maybe.map (\ (DefaultMember md) ->  
               md 
               |> createDimensionalHeader (calculateDefaultArea direction depth (getLastArea memberHeaders)))
      in
         (memberHeaders, defaultMemberHeader)


type MemberHeader = MemberHeader DimensionalHeader
type DefaultHeader = DefaultHeader DimensionalHeader
type AccumulatedHeader = 
   SimpleMember (NList MemberHeader)
   | TotalDefault  (DefaultHeader, NList MemberHeader)

addColumns: Direction -> Depth -> Dimension -> NList MemberHeader -> (List DimensionalHeader, Maybe DimensionalHeader)--List AccumulatedHeader
addColumns direction depth dimension headers =
   let 

      -- d:  MemberHeader -> DimensionalHeaderItem 
      -- d (MemberHeader (DimensionalHeader item)) =  item
      columnHeaders:MemberHeader -> (List DimensionalHeader, Maybe DimensionalHeader)
      columnHeaders (MemberHeader (DimensionalHeader item))  = 
         fromDimension direction depth item.area dimension
      nonWritableHeaders: NList MemberHeader 
      nonWritableHeaders =
         headers 
         |> NList.map (\(MemberHeader (DimensionalHeader item))  -> 
                           { item | write = False } 
                           |> DimensionalHeader 
                           |> MemberHeader)
   in
      columnHeaders headers.head 
         
   
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






   