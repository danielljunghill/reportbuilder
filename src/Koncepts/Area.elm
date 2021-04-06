module Koncepts.Area exposing (..)

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

verticalStartAdd: VerticalStart -> VerticalStart -> VerticalStart
verticalStartAdd (VerticalStart (Start s1)) (VerticalStart (Start s2)) =
   s1 + s2
   |> Start
   |> VerticalStart

verticalSpanAdd: VerticalSpan -> VerticalSpan -> VerticalSpan
verticalSpanAdd (VerticalSpan (Span s1)) (VerticalSpan (Span s2)) =
   s1 + s2
   |> Span
   |> VerticalSpan

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

horizontalStartAdd: HorizontalStart -> HorizontalStart -> HorizontalStart
horizontalStartAdd (HorizontalStart (Start s1)) (HorizontalStart (Start s2)) =
   s1 + s2
   |> Start
   |> HorizontalStart

horizontalSpanAdd: HorizontalSpan -> HorizontalSpan -> HorizontalSpan
horizontalSpanAdd (HorizontalSpan (Span s1)) (HorizontalSpan (Span s2)) =
   s1 + s2
   |> Span
   |> HorizontalSpan

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

depthInt (Depth depth) = depth

incrementDepth (Depth depth) = 
   depth + 1
   |> Depth

decrementDepth (Depth depth) =
   depth - 1
   |> Depth

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

emptyArea: Area
emptyArea =  
    {    
         horizontalStart = intToHorizontalStart 0
      ,  horizontalSpan = intToHorizontalSpan 0
      ,  verticalStart = intToVerticalStart 0
      ,  verticalSpan = intToVerticalSpan 0
    }

oneVerticalStart: VerticalStart 
oneVerticalStart =  1 |> Start |> VerticalStart

oneHorizontalStart: HorizontalStart
oneHorizontalStart = 1 |> Start |> HorizontalStart

oneVerticalSpan: VerticalSpan
oneVerticalSpan = 1 |> Span |> VerticalSpan

oneHorizontalSpan: HorizontalSpan
oneHorizontalSpan = 1 |> Span |> HorizontalSpan


zeroVerticalStart: VerticalStart 
zeroVerticalStart =  0 |> Start |> VerticalStart

zeroHorizontalStart: HorizontalStart
zeroHorizontalStart = 0 |> Start |> HorizontalStart

zeroVerticalSpan: VerticalSpan
zeroVerticalSpan = 0 |> Span |> VerticalSpan

zeroHorizontalSpan: HorizontalSpan
zeroHorizontalSpan = 0 |> Span |> HorizontalSpan

greaterThanZeroVerticalStart: Area -> Area
greaterThanZeroVerticalStart area =
      if (area.verticalStart |> verticalStartToStart) == Start 0 then 
         area |> setVerticalStart (Start 1)
      else area

greaterThanZeroHorizonalStart: Area -> Area
greaterThanZeroHorizonalStart area =
      if (area.horizontalStart |> horizontalStartToStart) == Start 0 then 
         area |> setHorizontalStart (Start 1)
      else area

greaterThanZeroStart: Area -> Area
greaterThanZeroStart = greaterThanZeroVerticalStart >> greaterThanZeroHorizonalStart

type alias Offset =
   {
         verticalStart: VerticalStart
      ,  horizontalStart: HorizontalStart
   }

emptyOffset: Offset
emptyOffset = 
   {
         verticalStart = zeroVerticalStart
      ,  horizontalStart = zeroHorizontalStart
   }

addVerticalStartToOffset: Offset -> VerticalStart -> Offset 
addVerticalStartToOffset offset vStart    =
   let
      startOffest : Start
      startOffest = offset.verticalStart |> verticalStartToStart   
      start : Start
      start = vStart |> verticalStartToStart  
   in
      { offset | verticalStart = start |> startAdd startOffest |> VerticalStart }

addHorizontalStartToOffset: Offset -> HorizontalStart -> Offset 
addHorizontalStartToOffset offset hStart  =
   let
      startOffest : Start
      startOffest = offset.horizontalStart |> horizontalStartToStart   
      start : Start
      start = hStart |> horizontalStartToStart  
   in
      { offset | horizontalStart = start |> startAdd startOffest |> HorizontalStart }


addHorizontalStart: HorizontalStart  -> Area -> Area
addHorizontalStart hs area =
   { area | horizontalStart = (area.horizontalStart |> horizontalStartAdd hs) }

addVerticalStart: VerticalStart  -> Area -> Area
addVerticalStart vs area =
   { area | verticalStart = (area.verticalStart |> verticalStartAdd vs) }

addHorizontalSpan: HorizontalSpan -> Area -> Area
addHorizontalSpan hs area =
   { area | horizontalSpan = (area.horizontalSpan |> horizontalSpanAdd hs) }

addVerticalSpan: VerticalSpan -> Area -> Area
addVerticalSpan vs area =
   { area | verticalSpan = (area.verticalSpan |> verticalSpanAdd vs) }

offsetArea: Offset -> Area -> Area 
offsetArea offset area =
      area
      |> addVerticalStart offset.verticalStart
      |> addHorizontalStart  offset.horizontalStart

