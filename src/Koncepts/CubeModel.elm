module Koncepts.CubeModel exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Area as Area 
import NList exposing (..)
import Koncepts.Model exposing (Factor(..),Member)

type Indent = Indent Int

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

type CubeRowOffset = CubeRowOffset Offset

type alias CubeColumns = 
    {
            columns: List CubeColumn
         ,  headers:List CubeHeader   --dimensionsToCubeColumnHeaders direction selection dimensions
         ,  offset: CubeRowOffset
    }

-- type alias CubeHeader  =
--     {
--             span: Span
--         ,   start: Start
--         ,   depth: Depth
--         ,   attributes: List String
--         ,   name: String
--         ,   isSelected: Bool
--     }


-- depthToStart (Depth depth) = depth |> Start

-- cubeHeaderToArea: Direction -> CubeHeader -> Area
-- cubeHeaderToArea direction cubeHeader =
--     case direction of
--         Vertical ->
--             Area.emptyArea
--             |> addVerticalSpan (cubeHeader.span |> VerticalSpan)
--             |> addVerticalStart (cubeHeader.start  |> VerticalStart)
--             |> addHorizontalStart ((depthToStart cubeHeader.depth) |> HorizontalStart)  
--             |> addHorizontalSpan (1 |> Span |> HorizontalSpan)

--         Horizontal ->
--             Area.emptyArea
--             |> addVerticalSpan (1 |> Span |> VerticalSpan) 
--             |> addVerticalStart  ((depthToStart cubeHeader.depth)|> VerticalStart)
--             |> addHorizontalStart (cubeHeader.start|> HorizontalStart) 
--             |> addHorizontalSpan (cubeHeader.span |> HorizontalSpan)


-- type alias CubeHeader  =
--     {
--             span: Span
--         ,   start: Start
--         ,   depth: Depth
--         ,   attributes: List String
--         ,   name: String
--         ,   isSelected: Bool
--     }


-- depthToStart (Depth depth) = depth |> Start

-- cubeHeaderToArea: Direction -> CubeHeader -> Area
-- cubeHeaderToArea direction cubeHeader =
--     case direction of
--         Vertical ->
--             Area.emptyArea
--             |> addVerticalSpan (cubeHeader.span |> VerticalSpan)
--             |> addVerticalStart (cubeHeader.start  |> VerticalStart)
--             |> addHorizontalStart ((depthToStart cubeHeader.depth) |> HorizontalStart)  
--             |> addHorizontalSpan (1 |> Span |> HorizontalSpan)

--         Horizontal ->
--             Area.emptyArea
--             |> addVerticalSpan (1 |> Span |> VerticalSpan) 
--             |> addVerticalStart  ((depthToStart cubeHeader.depth)|> VerticalStart)
--             |> addHorizontalStart (cubeHeader.start|> HorizontalStart) 
--             |> addHorizontalSpan (cubeHeader.span |> HorizontalSpan)


-- type CubeColumn = CubeColumn(NList Member)

-- type CubeRowOffset = CubeRowOffset Offset

-- type alias CubeColumns = 
--     {
--             columns: List CubeColumn
--          ,  headers:List CubeHeader   --dimensionsToCubeColumnHeaders direction selection dimensions
--          ,  offset: CubeRowOffset
--     }


-- type alias CubeHeader  =
--     {
--             span: Span
--         ,   start: Start
--         ,   depth: Depth
--         ,   attributes: List String
--         ,   name: String
--         ,   isSelected: Bool
--     }


-- type Indent = Indent Int
-- type alias CubeHeader  =
--     {
--             span: Span
--         ,   start: Start
--         ,   depth: Depth
--         ,   attributes: List String
--         ,   name: String
--         ,   isSelected: Bool
--         ,   indent: MaybeIndent
--     }

-- depthToStart (Depth depth) = depth |> Start

-- cubeHeaderToArea: Direction -> CubeHeader -> Area
-- cubeHeaderToArea direction cubeHeader =
--     case direction of
--         Vertical ->
--             Area.emptyArea
--             |> addVerticalSpan (cubeHeader.span |> VerticalSpan)
--             |> addVerticalStart (cubeHeader.start |> VerticalStart)
--             |> addHorizontalStart ((depthToStart cubeHeader.depth) |> HorizontalStart)
--             |> addHorizontalSpan oneHorizontalSpan

--         Horizontal ->
--             Area.emptyArea
--             |> addVerticalSpan oneVerticalSpan
--             |> addVerticalStart ((depthToStart cubeHeader.depth) |> VerticalStart)
--             |> addHorizontalStart (cubeHeader.start|> HorizontalStart)
--             |> addHorizontalSpan (cubeHeader.span |> HorizontalSpan)

-- type CubeColumn = CubeColumn(NList Member)

-- type CubeRowOffset = CubeRowOffset Offset

-- type alias CubeColumns = 
--     {
--             columns: List CubeColumn
--          ,  headers:List CubeHeader   --dimensionsToCubeColumnHeaders direction selection dimensions
--          ,  offset: CubeRowOffset
--     }