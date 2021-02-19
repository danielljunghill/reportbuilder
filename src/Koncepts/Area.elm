module Koncepts.Area exposing (..)
import Koncepts.Lines exposing(..
import Koncepts.Lines


type Area =
   {
      verticalLine: VerticalLine
      horizontalLine: HorizontalLine
   }

incrementVerticalStart: Area -> Area
incrementVerticalStart area =
   {
       area | VerticalLine = Lines.incrementVerticalStart area.verticalLine
   }

incrementHorizontalStart: Area -> Area
incrementHorizontalStart area =
   {
       area | horizonalStart = Lines.incrementHorizontalStart area.horizontalLine
   }

setHorizontalStart: Lines.Start -> Area -> Area  
setHorizontalStart start area =
   {
      area | horizonalStart = area.horizontalLine |> Lines.setHorizontalStart start
   }

setVerticalStart: Lines.Start -> Area -> Area  
setVerticalStart start area =
   {
      area | verticalStart = area.verticalLine |> Lines.setVerticalStart start
   }


setHorizontalSpan: Lines.Span -> Area -> Area  
setHorizontalSpan: span area =
   {
      area | horizonalStart = area.horizontalLine |> Lines.setHorizontalSpan span
   }

setVerticalSpan: Lines.Span -> Area -> Area  
setVerticalSpan span area =
   {
      area | verticalStart = area.verticalLine |> Lines.setVerticalSpan span
   }

vertialStart: Area -> Start
verticalStart area =
   Lines.vertialStart area.verticalLine

horizontaStart: Area -> Start
horizontaStart area =
   Lines.horizonalStart area.horizontalLine

verticalSpan: Area -> Span
verticalSpan area =
   Lines.verticalSpan area.verticalLine

horizontalSpan: Area -> Span
horizontalSpan area =
   Lines.horizonalSpan area.horizontalLine
