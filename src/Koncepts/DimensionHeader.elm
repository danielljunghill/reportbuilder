module Koncepts.DimensionHeader exposing (..)
import Koncepts.Dimensionalkoncept exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.Lines exposing (..)
import Koncepts.Lines as Lines
import Lists as Lists

type alias DimensionalHeaderItem =
   {
         area: Area
      ,  member: Member
      ,  write: Bool
   }

type DimensionalHeader = DimensionalHeader DimensionalHeaderItem


calculateSpan: Dimension -> Span -> Span
calculateSpan dimension (Span span)   =
   case dimension of
      DimensionWithDefault (_, domain) -> (span - 1) // (domain.members |> List.length) |> Span
      DimensionWithoutDefault (domain) -> span // (domain.members |> List.length) |> Span
   

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
calculateDefaultArea direction depth area =
   
fromDimension: Direction -> Depth -> Area -> Dimension -> (List DimensionalHeader, Maybe DimensionalHeader )
fromDimension direction depth area dimension =
                
      let
         dms: List DomainMember
         dms = dimensionMembers.members dimension
         dm: Maybe DefaultMember
         dm = memberDefault dimension
         memberHeaders: List DimensionalHeader
         memberHeaders = 
            dms
            |> Lists.mapi 
               (\ i (DomainMember m) ->  
                     m.name 
                     |> createMember (calculateArea direction area i)
                     |> DimensionalHeader)

         getLastArea: List DimensionalHeader -> Area 
         getLastArea headers = 
            headers 
            |> Lists.rev 
            |> List.head 
            |> (\ (DimensionalHeader item) -> item.area)

         defaultMemberHeader: Maybe DimensionalHeader
         defaultMemberHeader =
            dm
            |> Maybe.map (\ (DefaultMember md) ->  
               -- let area = Area.total direction depth (getLastArea memberHeaders)
               md.name 
               |> createMember (Factor 1) (Area.total direction depth (getLastArea memberHeaders)) 
               |> DimensionalHeader)
      in
         (memberHeaders, defaultMemberHeader)
     
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






   