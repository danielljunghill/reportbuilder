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

-- calculateStart: Int -> Span -> Start -> Start
-- calculateStart ordinal (Span span) (Start start)  =
--          start + span * ordinal
--          |> Start

calculateSpanForDimensions: List Dimension -> ColumnSpan
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
        |> ColumnSpan

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

createCubeHeaders: Maybe Selection  -> ColumnSpan -> List MemberHeader -> List CubeHeader -> List CubeHeader
createCubeHeaders selection columnSpan members parents =
    let 
    -- ANVÄNDA AREA
        newHeader: Int -> CubeHeader -> MemberHeader -> CubeHeader 
        -- newHeader index parent memberHeader =
        --     {
        --             column =  (index * (spanInt span) + Area.startInt parent.column) |> Start
        --         ,   columnSpan =  columnSpan 
        --         ,   row =  parent.row |> incRow 
        --         ,   rowSpan = Span 1
        --         ,   name = memberHeader.member.name
        --         ,   attributes = memberHeader |> addTotalAttribute
        --         ,   isSelected = parent |> Just |> memberIsSelected selection memberHeader  
        --         ,   indent = Nothing
        --     }
        newHeader index parentHeader memberHeader =
            let 
                area =
                    {
                            column =  (index * (intColumnSpan columnSpan) + (intColumn parentHeader.area.column)) |> Column
                        ,   columnSpan = columnSpan
                        ,   row =  parentHeader.area.row |> incRow
                        ,   rowSpan = RowSpan 1
                    }
            in
                {
                        area = area
                    ,   attributes = memberHeader |> addTotalAttribute
                    ,   isSelected = 
                            parentHeader 
                            |> Just 
                            |> memberIsSelected selection memberHeader 
                    ,   indent = Nothing 
                    ,   name = memberHeader.member.name
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


createFirstCubeHeaders:Maybe Selection -> ColumnSpan -> List MemberHeader -> List CubeHeader
createFirstCubeHeaders selection colSpan members  =
    let 
        createHeader index memberHeader =
            let
                area =
                        
                    {
                            column =  (index * (intColumnSpan colSpan) + 1) |> Column
                        ,   columnSpan =  colSpan
                        ,   row =   Row 1
                        ,   rowSpan = RowSpan 1
                    }
            in
                {
                        area = area
                    ,   name = memberHeader.member.name
                    ,   attributes = addTotalAttribute memberHeader
                    ,   isSelected = 
                            Nothing 
                            |> memberIsSelected selection memberHeader   
                    ,   indent = Nothing
                }
            
    in
        members
        |> Lists.mapi (\index member -> createHeader index member) 

dimensionsToCubeHeaders: Maybe Selection -> List Dimension  -> List CubeHeader
dimensionsToCubeHeaders selection dims =
    let
        totalSpanForDimensions = dims |> calculateSpanForDimensions
        recDimensionToHeaders: ColumnSpan ->  List CubeHeader -> List Dimension -> List CubeHeader
        recDimensionToHeaders (ColumnSpan parentSpan) state dimensions =
            case dimensions of
                [] -> []
                dimension :: tail ->
                    let 
                        -- create headers from dimension
                        members = dimensionToHeaderMembers dimension
                        -- span i parent span divided by count of members
                        span = parentSpan // (List.length members) |> ColumnSpan
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


         dimensions = 
    dimensions
    |> NList.cross [] dimensionToMembers
    |> List.map CubeColumn



cubeRowOffsetToOffset: CubeRowOffset -> Offset
cubeRowOffsetToOffset (CubeRowOffset offset) = offset

calculateCubeColumns:  Maybe Selection ->  List Dimension -> CubeColumns  
calculateCubeColumns selection dimensions =
   let

      offset : CubeRowOffset
      offset =
        zeroOffset
        |> addRowToOffset (dimensions |> List.length |> Row )
        |> CubeRowOffset
   in
      {
            columns = dimensionToCubeColumns dimensions
         ,  headers = dimensionsToCubeHeaders selection dimensions     --dimensionsToCubeColumnHeaders direction selection dimensions
         ,  offset = offset
      }


