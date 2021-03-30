module Koncepts.CubeDimension2 exposing (..)
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

type alias MemberHeader =
    {
            member: Member
        ,   isTotal: Bool
    }



calculateStart: Int -> Span -> Start -> Start
calculateStart ordinal (Span span) (Start start)  =
         start + span * ordinal
         |> Start



type Depth = Depth Int

depthInt (Depth depth) = depth

incrementDepth (Depth depth) = 
    depth + 1
    |> Depth

depthToStart (Depth depth) = Start depth

type alias Header a =
    {
            span: Span
        ,   start: Start
        ,   depth: Depth
        ,   items: NList a
    }

    
calculateAreaForHeader: Direction -> Header a -> Area 
calculateAreaForHeader direction header =
    case direction of
        Horizontal ->
            Area.emptyArea
            |> addHorizontalStart (HorizontalStart header.start)
            |> addHorizontalSpan (HorizontalSpan header.span)
            |> addVerticalSpan (1 |> Span |> VerticalSpan)
            |> addVerticalStart (header.depth |> depthToStart |> VerticalStart)
        Vertical ->
            Area.emptyArea
            |> addVerticalStart (VerticalStart header.start)
            |> addVerticalSpan (VerticalSpan header.span)
            |> addHorizontalSpan (1 |> Span |> HorizontalSpan)
            |> addHorizontalStart (header.depth |> depthToStart |> HorizontalStart)


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

dimensionToHeaderMembers: Dimension -> List MemberHeader 
dimensionToHeaderMembers dimension =
    let 
        memberToHeader isTotal member =
            { 
                    isTotal = isTotal
                ,   member = member
            }
    in
    case dimension of
        DimensionWithDefault (DefaultMember dm,domain) ->
            domain.members 
            |> NList.map (\(DomainMember m) -> memberToHeader False m)
            |> NList.addLast (memberToHeader True dm)
            |> NList.toList
        DimensionWithoutDefault domain ->
            domain.members 
            |> NList.map (\(DomainMember m) -> memberToHeader False m)
            |> NList.toList

--            
createMemberHeaders: Span -> List MemberHeader -> List (Header MemberHeader) -> List (Header MemberHeader)
createMemberHeaders (Span spanParent) members parents =
    let 
        newHeader: Int -> Span -> Header MemberHeader -> MemberHeader -> Header MemberHeader 
        newHeader index (Span span) parent memberHeader =
            {
                    start =  (index * span + Area.startInt parent.start) |> Start
                ,   span =  span |> Span
                ,   items =  NList.addFirst memberHeader parent.items 
                ,   depth =  ((depthInt parent.depth) + 1) |> Depth
            }
        membersCount = List.length members
        newSpan = spanParent // membersCount |> Span

        createHeaders:List (Header MemberHeader) -> Header MemberHeader ->  List (Header MemberHeader) 
        createHeaders state header = 
            let 
                newHeaders =
                    members 
                    |> Lists.mapi (\index m -> newHeader index newSpan header m)
            in
                state ++ newHeaders

    in
        parents
        |> Lists.fold createHeaders []


dimensionsToHeaders: List Dimension  -> List (Header MemberHeader)
dimensionsToHeaders dims =
    let
        totalSpanForDimensions = dims |> calculateSpanForDimensions
        recDimensionToHeaders: Span ->  List (Header MemberHeader) -> List Dimension -> List (Header MemberHeader)
        recDimensionToHeaders (Span parentSpan) state dimensions =
            case dimensions of
                [] -> []
                dimension :: tail ->
                    let 
                        -- create headers from dimension
                        members = dimensionToHeaderMembers dimension
                        -- span i parent span divided by count of members
                        span = parentSpan // (List.length members) |> Span
                        -- new State = List of (Header MemberHeader)
                        newState =
                            if List.isEmpty state then
                                members
                                |> Lists.mapi (\index member ->  { items = NList.create member, start = (index * (spanInt span) + 1) |> Start , span = span , depth = Depth 1})
                            else                              
                                createMemberHeaders (Span parentSpan) members state
                    in
                        newState ++ recDimensionToHeaders span newState tail 
    in
        dims 
        |> recDimensionToHeaders totalSpanForDimensions []


type alias CubeColumnHeader =
   {
         isTotal: Bool
      ,  area: Area
      ,  member: Member
      ,  isSelected: Bool
   }

dimensionsToCubeColumnHeaders: Direction -> List Factor -> List Dimension -> List CubeColumnHeader 
dimensionsToCubeColumnHeaders direction factors dimensions =
    let
        mapToCubeColumnHeader: Header MemberHeader -> CubeColumnHeader 
        mapToCubeColumnHeader header =
            let
               filteredMembers: List MemberHeader
               filteredMembers =
                  header.items 
                  |> NList.toList
                  |> List.filter (\m -> (Lists.contains m.member.factor factors)) 

            in 

                {
                        isSelected = (List.length filteredMembers) == (NList.length header.items)
                    ,   area = calculateAreaForHeader direction header
                    ,   member = header.items.head.member
                    ,   isTotal = header.items.head.isTotal
                }     
    in
        dimensions
        |> dimensionsToHeaders
        |> List.map mapToCubeColumnHeader

type CubeColumn = CubeColumn (NList Member)

dimensionToMembers dimension =
    case dimension of
        DimensionWithDefault (DefaultMember dm,domain) ->  
            domain.members 
            |> NList.map (\(DomainMember m) -> m)
            |> NList.addLast dm 
        DimensionWithoutDefault (domain)-> 
            domain.members 
            |> NList.map (\(DomainMember m) -> m)


dimensionToCubeColumns dimensions = 
    dimensions
    |> NList.cross [] dimensionToMembers
    |> List.map CubeColumn

type alias CubeColumns = 
   {
         offset: CubeRowOffset
      ,  columns: List CubeColumn  
      ,  headers: List CubeColumnHeader 
   }
type CubeRowOffset = CubeRowOffset Offset
cubeRowOffsetToOffset: CubeRowOffset -> Offset
cubeRowOffsetToOffset (CubeRowOffset offset) = offset

calculateCubeColumns:  Direction -> List Factor ->  List Dimension -> CubeColumns  
calculateCubeColumns direction selection dimensions =
   let

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
            columns = dimensionToCubeColumns dimensions
         ,  headers = dimensionsToCubeColumnHeaders direction selection dimensions
         ,  offset = offset |>  CubeRowOffset
      }