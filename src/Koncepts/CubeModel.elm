module Koncepts.CubeModel exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Area as Area 
import NList exposing (..)
import Koncepts.Model exposing (Factor(..),Member, membersAsFactor)

type Indent = Indent Int
incrementIndent (Indent indent) =
    indent + 1
    |> Indent

type alias CubeHeader  =
    {
            column: Start
        ,   columnSpan: Span
        ,   row: Start
        ,   rowSpan: Span
        ,   attributes: List String
        ,   name: String
        ,   isSelected: Bool
        ,   indent: Maybe Indent
    }

-- depthToStart (Depth depth) = depth |> Start

cubeHeaderToArea: Direction -> CubeHeader -> Area
cubeHeaderToArea direction cubeHeader =
    case direction of
        Vertical ->
            Area.emptyArea
            |> addVerticalSpan (cubeHeader.columnSpan |> VerticalSpan)
            |> addVerticalStart (cubeHeader.column |> VerticalStart)
            |> addHorizontalStart (cubeHeader.row|> HorizontalStart)
            |> addHorizontalSpan (cubeHeader.rowSpan |> HorizontalSpan)

        Horizontal ->
            Area.emptyArea
            |> addVerticalSpan (cubeHeader.rowSpan |> VerticalSpan)
            |> addVerticalStart (cubeHeader.row |> VerticalStart)
            |> addHorizontalStart (cubeHeader.column|> HorizontalStart)
            |> addHorizontalSpan (cubeHeader.columnSpan |> HorizontalSpan)

type CubeColumn = CubeColumn(NList Member)

cubeColumnAsFactor: CubeColumn -> Factor
cubeColumnAsFactor (CubeColumn members) =
    members 
    |> NList.toList 
    |> membersAsFactor 

type CubeRowOffset = CubeRowOffset Offset

type alias CubeColumns = 
    {
            columns: List CubeColumn
         ,  headers:List CubeHeader   
         ,  offset: CubeRowOffset
    }

