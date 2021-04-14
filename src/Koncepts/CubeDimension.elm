module Koncepts.CubeDimension exposing (..)
import Koncepts.CubeKoncept exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Area as Area
import Koncepts.Model exposing (..)
import Koncepts.CubeModel exposing (..)
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

type MemberHeaders = MemberHeaders (NList MemberHeader)

membersInMemberHeaders (MemberHeaders n) = n 
countMemberHeaders (MemberHeaders n) = n |> NList.length 


addMemberToMemberHeaders: MemberHeader -> MemberHeaders -> MemberHeaders
addMemberToMemberHeaders mh (MemberHeaders headers) =
    headers 
    |> NList.addFirst mh
    |> MemberHeaders

calculateStart: Int -> Span -> Start -> Start
calculateStart ordinal (Span span) (Start start)  =
         start + span * ordinal
         |> Start

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

addTotalAttribute: MemberHeader -> List String 
addTotalAttribute memberHeader =
    if memberHeader.isTotal then [ "total" ]
    else []


memberIsSelected: Maybe Selection -> MemberHeader -> Maybe CubeHeader -> Bool
memberIsSelected maybeSelection memberHeader maybeParent  =

   let 
        isMemberInSelection member =
            case maybeSelection of
                Just selection ->
                    selection 
                    |> factorsInSelection 
                    |> Lists.contains member.factor
                Nothing -> False
        isSelected parent =
            if parent.isSelected then
                isMemberInSelection memberHeader.member
            else False
    in
        maybeParent 
        |> Maybe.map isSelected 
        |> Maybe.withDefault (isMemberInSelection memberHeader.member)

createCubeHeaders: Maybe Selection  -> Span -> List MemberHeader -> List CubeHeader -> List CubeHeader
createCubeHeaders selection span members parents =
    let 
        newHeader: Int -> CubeHeader -> MemberHeader -> CubeHeader 
        newHeader index parent memberHeader =
            {
                    column =  (index * (spanInt span) + Area.startInt parent.column) |> Start
                ,   columnSpan =  span 
                ,   row =  parent.row |> startIncrement 
                ,   rowSpan = Span 1
                ,   name = memberHeader.member.name
                ,   attributes = memberHeader |> addTotalAttribute
                ,   isSelected = parent |> Just |> memberIsSelected selection memberHeader  
                ,   indent = Nothing
            }

        createHeaders:List CubeHeader -> CubeHeader ->  List CubeHeader
        createHeaders state header = 
            let 
                newHeaders =
                    members 
                    |> Lists.mapi (\index m -> newHeader index  header m)
            in
                state ++ newHeaders
    in
        parents
        |> Lists.fold createHeaders []


createFirstCubeHeaders:Maybe Selection  -> Span -> List MemberHeader -> List CubeHeader
createFirstCubeHeaders selection span members  =
    let 
        createHeader index memberHeader =
            {
                    column =  (index * (spanInt span) + 1) |> Start
                ,   columnSpan =  span
                ,   name = memberHeader.member.name
                ,   row =   Start 1
                ,   rowSpan = Span 1
                ,   attributes = addTotalAttribute memberHeader
                ,   isSelected = Nothing |> memberIsSelected selection memberHeader  
                ,   indent = Nothing
            }
    in
        members
        |> Lists.mapi (\index member -> createHeader index member) 

dimensionsToCubeHeaders: Maybe Selection -> List Dimension  -> List CubeHeader
dimensionsToCubeHeaders selection dims =
    let
        totalSpanForDimensions = dims |> calculateSpanForDimensions
        recDimensionToHeaders: Span ->  List CubeHeader -> List Dimension -> List CubeHeader
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
                                createFirstCubeHeaders selection span members
                            else                              
                                createCubeHeaders selection span members state
                    in
                        newState ++ recDimensionToHeaders span newState tail 
    in
        dims 
        |> recDimensionToHeaders totalSpanForDimensions []


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



cubeRowOffsetToOffset: CubeRowOffset -> Offset
cubeRowOffsetToOffset (CubeRowOffset offset) = offset

calculateCubeColumns:  Direction -> Maybe Selection ->  List Dimension -> CubeColumns  
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
         ,  headers = dimensionsToCubeHeaders selection dimensions     --dimensionsToCubeColumnHeaders direction selection dimensions
         ,  offset = offset |>  CubeRowOffset
      }


