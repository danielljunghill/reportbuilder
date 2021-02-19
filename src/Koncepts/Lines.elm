module Koncepts.Area exposing (..)

type Direction =
   Vertical
   | Horizontal

type Span = Span Int
spanInt: Span -> Int
spanInt (Span span) = span

type Start = Start Int
startInt: Start -> Int
startInt (Start start) -> start

type Line = 
   { 
         start: Start
      ,  span: Span
   }

lineSpan: Line -> Int
lineSpan line = line.span |>  spanInt

lineStart: Line
lineStart line = line.start|> startInt

type HorizontalLine = HorizontalLine Line

type VerticalLine = VerticalLine Line

verticalSpan: VerticalLine -> Span
verticalSpan (VerticalLine line) = line.span

horizonalSpan: HorizontalLine -> Span
horizonalSpan (HorizontalLine line) = line.span

verticalStart: VerticalLine -> Span
verticalStart (VerticalLine line) = line.span

horizonalStart: HorizontalLine -> Span
horizonalStart (HorizontalLine line) = line.span

   