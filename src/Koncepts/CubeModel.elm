module Koncepts.CubeModel exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Area as Area 
import NList exposing (..)
import Koncepts.Model exposing (Factor(..),Member)



type alias CubeHeader  =
    {
            span: Span
        ,   start: Start
        ,   depth: Depth
        ,   attributes: List String
        ,   name: String
        ,   isSelected: Bool
    }


depthToStart (Depth depth) = depth |> Start

cubeHeaderToArea: Direction -> CubeHeader -> Area
cubeHeaderToArea direction cubeHeader =
    case direction of
        Vertical ->
            Area.emptyArea
            |> addVerticalSpan (cubeHeader.span |> VerticalSpan)
            |> addVerticalStart (cubeHeader.start  |> VerticalStart)
            |> addHorizontalStart ((depthToStart cubeHeader.depth) |> HorizontalStart)  
            |> addHorizontalSpan (1 |> Span |> HorizontalSpan)

        Horizontal ->
            Area.emptyArea
            |> addVerticalSpan (1 |> Span |> VerticalSpan) 
            |> addVerticalStart  ((depthToStart cubeHeader.depth)|> VerticalStart)
            |> addHorizontalStart (cubeHeader.start|> HorizontalStart) 
            |> addHorizontalSpan (cubeHeader.span |> HorizontalSpan)


type CubeColumn = CubeColumn(NList Member)


type CubeRowOffset = CubeRowOffset Offset

type alias CubeColumns = 
    {
            columns: List CubeColumn
         ,  headers:List CubeHeader   --dimensionsToCubeColumnHeaders direction selection dimensions
         ,  offset: CubeRowOffset
    }