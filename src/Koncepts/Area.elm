module Koncepts.Area exposing (..)
-- import Koncepts.Lines exposing(..)
-- import Koncepts.Lines as Lines


-- mapBase: (Int -> Int) -> (a -> int) -> (int -> a) -> a
-- mapBase f a b =
--    a >> f >> b

type Span = Span Int
spanInt: Span -> Int
spanInt (Span span) = span

spanAdd: Span -> Span -> Span
spanAdd (Span span1) (Span span2) = Span (span1 + span2)

spanMap: (Int -> Int) -> Span -> Span
spanMap f (Span span) =
   f span
   |> Span

spanIncrement: Span -> Span
spanIncrement = spanMap (\i -> i + 1) 

type VerticalSpan = VerticalSpan Span
verticalSpanMap:  (Span -> Span) -> VerticalSpan -> VerticalSpan
verticalSpanMap f (VerticalSpan span) =
   span   
   |> f 
   |> VerticalSpan

verticalSpanToSpan: VerticalSpan -> Span
verticalSpanToSpan (VerticalSpan span) = span
   
verticalSpanToInt: VerticalSpan -> Int
verticalSpanToInt = verticalSpanToSpan >> spanInt

intToVerticalSpan: Int -> VerticalSpan
intToVerticalSpan = Span >> VerticalSpan

type HorizontalSpan = HorizontalSpan Span
horizontalSpanMap:  (Span -> Span) -> HorizontalSpan -> HorizontalSpan
horizontalSpanMap f (HorizontalSpan span) =
   span   
   |> f 
   |> HorizontalSpan

horizontalSpanToSpan: HorizontalSpan -> Span  
horizontalSpanToSpan (HorizontalSpan span) = span

horizontalSpanToInt: HorizontalSpan -> Int
horizontalSpanToInt = horizontalSpanToSpan >> spanInt

intToHorizontalSpan: Int -> HorizontalSpan
intToHorizontalSpan = Span >> HorizontalSpan

type Start = Start Int
startInt: Start -> Int
startInt (Start start) = start

startMap: (Int -> Int) -> Start -> Start
startMap f (Start start) =
   f start
   |> Start

startIncrement: Start -> Start
startIncrement = startMap (\i -> i + 1) 
  
startAdd: Start -> Start -> Start
startAdd (Start start1) (Start start2) = Start (start1 + start2)

startSpan: (Int -> Int -> Int) -> Start -> Span -> Int
startSpan f (Start start) (Span span) =
   f start span

spanStart: (Int -> Int -> Int) -> Span -> Start -> Int
spanStart f  (Span span) (Start start)=
   f start span

type VerticalStart = VerticalStart Start
verticalStartMap:  (Start -> Start) -> VerticalStart -> VerticalStart
verticalStartMap f (VerticalStart start) =
   start   
   |> f 
   |> VerticalStart

verticalStartToStart: VerticalStart -> Start
verticalStartToStart (VerticalStart start) = start  
   
verticalStartToInt: VerticalStart -> Int
verticalStartToInt = verticalStartToStart >> startInt

intToVerticalStart: Int -> VerticalStart
intToVerticalStart = Start >> VerticalStart

type HorizontalStart = HorizontalStart Start
horizontalStartMap:  (Start -> Start) -> HorizontalStart -> HorizontalStart
horizontalStartMap f (HorizontalStart start) =
   start   
   |> f 
   |> HorizontalStart

horizontalStartToStart: HorizontalStart -> Start
horizontalStartToStart (HorizontalStart start) = start

horizontalStartToInt: HorizontalStart -> Int
horizontalStartToInt = horizontalStartToStart >> startInt

intToHorizontalStart: Int -> HorizontalStart
intToHorizontalStart = Start >> HorizontalStart


type alias Area =
   {
         horizontalStart: HorizontalStart
      ,  horizontalSpan: HorizontalSpan
      ,  verticalStart: VerticalStart
      ,  verticalSpan: VerticalSpan
   }


type Direction =
   Vertical
   | Horizontal

type Depth = Depth Int

incrementVerticalStart: Area -> Area
incrementVerticalStart area =
   {
       area | verticalStart = area.verticalStart |> (verticalStartMap startIncrement)
   }

incrementHorizontalStart: Area -> Area
incrementHorizontalStart area =
   {
       area | horizontalStart = area.horizontalStart |> (horizontalStartMap startIncrement)
   }

setHorizontalStart: Start -> Area -> Area  
setHorizontalStart start area =
   {
      area | horizontalStart = HorizontalStart start
   }

setVerticalStart: Start -> Area -> Area  
setVerticalStart start area =
   {
      area | verticalStart = VerticalStart start
   }

setHorizontalSpan: Span -> Area -> Area  
setHorizontalSpan span area =
   {
      area | horizontalSpan = HorizontalSpan span
   }

setVerticalSpan: Span -> Area -> Area  
setVerticalSpan span area =
   {
      area | verticalSpan = VerticalSpan span
   }

verticalStart: Area -> Start
verticalStart area =
   area.verticalStart |> verticalStartToStart

horizontalStart: Area -> Start
horizontalStart area =
   area.horizontalStart |> horizontalStartToStart

verticalSpan: Area -> Span
verticalSpan area =
   area.verticalSpan |> verticalSpanToSpan

horizontalSpan: Area -> Span
horizontalSpan area =
   area.horizontalSpan |> horizontalSpanToSpan

emptyArea: Area
emptyArea =  
    {    
         horizontalStart = intToHorizontalStart 0
      ,  horizontalSpan = intToHorizontalSpan 0
      ,  verticalStart = intToVerticalStart 0
      ,  verticalSpan = intToVerticalSpan 0
    }






