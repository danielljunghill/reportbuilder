module Koncepts.Lines exposing (..)

type Span = Span Int
spanInt: Span -> Int
spanInt (Span span) = span

type Start = Start Int
startInt: Start -> Int
startInt (Start start) = start

type alias Line = 
   { 
         start: Start
      ,  span: Span
   }

lineSpan: Line -> Int
lineSpan line = line.span |>  spanInt

lineStart: Line -> Int
lineStart line = line.start |> startInt

addSpanToStart: Start -> Span -> Start
addSpanToStart (Start start) (Span span) =
   start + span 
   |> Start

incrementStart: Start -> Start
incrementStart (Start start) = 
   start + 1
   |> Start

incrementSpan: Span -> Span
incrementSpan (Span span) = 
   span + 1
   |> Span
type HorizontalLine = HorizontalLine Line

type VerticalLine = VerticalLine Line

verticalSpan: VerticalLine -> Span
verticalSpan (VerticalLine line) = line.span

horizontalSpan: HorizontalLine -> Span
horizontalSpan (HorizontalLine line) = line.span

verticalStart: VerticalLine -> Start
verticalStart (VerticalLine line) = line.start

horizontalStart: HorizontalLine -> Start
horizontalStart (HorizontalLine line) = line.start

verticalSetStart: Start -> VerticalLine -> VerticalLine
verticalSetStart start (VerticalLine line)  = 
   { line | start = start } 
   |> VerticalLine

verticalSetSpan: Span -> VerticalLine -> VerticalLine
verticalSetSpan span (VerticalLine line)  =
      { line | span = span } 
      |> VerticalLine

horizontalSetStart: Start -> HorizontalLine ->  HorizontalLine
horizontalSetStart start (HorizontalLine line)  = 
   { line | start = start } 
   |> HorizontalLine

horizontalSetSpan: Span ->  HorizontalLine -> HorizontalLine
horizontalSetSpan span (HorizontalLine line)  =
      { line | span = span} 
      |> HorizontalLine

horizontalIncrementStart: HorizontalLine -> HorizontalLine
horizontalIncrementStart (HorizontalLine line) = 
    { line | start = line.start |> incrementStart }
    |> HorizontalLine


verticalIncrementStart: VerticalLine -> VerticalLine
verticalIncrementStart (VerticalLine line) = 
   { line | start = line.start |> incrementStart }
   |> VerticalLine

horizontalIncrementStartWithSpan: HorizontalLine -> HorizontalLine
horizontalIncrementStartWithSpan (HorizontalLine line) =
    { line | start = addSpanToStart line.start line.span }
    |> HorizontalLine


verticalIncrementStartWithSpan: VerticalLine -> VerticalLine
verticalIncrementStartWithSpan (VerticalLine line) = 
    { line | start = addSpanToStart line.start line.span }
    |> VerticalLine

empty: Line
empty =
   { 
         span = Span 0
      ,  start = Start 0
   }


   