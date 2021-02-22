module Koncepts.Area exposing (..)
import Koncepts.Lines exposing(..)
import Koncepts.Lines as Lines


type alias Area =
   {
         verticalLine: VerticalLine
      ,  horizontalLine: HorizontalLine
   }

type Direction =
   Vertical
   | Horizontal

type Depth = Depth Int

incrementVerticalStart: Area -> Area
incrementVerticalStart area =
   {
       area | verticalLine = Lines.verticalIncrementStart area.verticalLine
   }

incrementHorizontalStart: Area -> Area
incrementHorizontalStart area =
   {
       area | horizontalLine = Lines.horizontalIncrementStart area.horizontalLine
   }

setHorizontalStart: Lines.Start -> Area -> Area  
setHorizontalStart start area =
   {
      area | horizontalLine = area.horizontalLine |> Lines.horizontalSetStart start
   }

setVerticalStart: Lines.Start -> Area -> Area  
setVerticalStart start area =
   {
      area | verticalLine = area.verticalLine |> Lines.verticalSetStart start
   }

setHorizontalSpan: Lines.Span -> Area -> Area  
setHorizontalSpan span area =
   {
      area | horizontalLine = area.horizontalLine |> Lines.horizontalSetSpan span
   }

setVerticalSpan: Lines.Span -> Area -> Area  
setVerticalSpan span area =
   {
      area | verticalLine = area.verticalLine |> Lines.verticalSetSpan span
   }

verticalStart: Area -> Start
verticalStart area =
   area.verticalLine |> Lines.verticalStart 

horizontaStart: Area -> Start
horizontaStart area =
   Lines.horizontalStart area.horizontalLine

verticalSpan: Area -> Span
verticalSpan area =
   Lines.verticalSpan area.verticalLine

horizontalSpan: Area -> Span
horizontalSpan area =
   Lines.horizontalSpan area.horizontalLine

emptyArea: Area
emptyArea =  
    {    
            verticalLine = VerticalLine Lines.empty
         ,  horizontalLine = HorizontalLine Lines.empty
    }