module Koncepts.DimensionalHeader exposing (..)
import Koncepts.Dimensionalkoncept exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Area as Area
import Koncepts.Model exposing (..)
import Lists as Lists
import NList exposing (..)
import Tuple exposing (..)
import Msg 
import Msg exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

type alias DimensionHeaderItem =
   {

         area: Area
      ,  member: NList Member 
   }

type DimensionHeader = DimensionHeader DimensionHeaderItem
itemFromDimensionalHeader: DimensionHeader -> DimensionHeaderItem
itemFromDimensionalHeader (DimensionHeader item) = item

createDimensionHeaderItem: Area ->  List Member -> Member -> DimensionHeaderItem
createDimensionHeaderItem a pm m  =
   {
            area = a
         ,  member = NList.create2 m pm

   }

createDimensionHeader : Area -> List Member -> Member -> DimensionHeader
createDimensionHeader a pm m  =
   createDimensionHeaderItem a pm m 
   |> DimensionHeader

calculateSpan: Dimension -> Span -> Span
calculateSpan dimension (Span span)   =
   case dimension of
      DimensionWithDefault (_, domain) -> (span - 1) // (domain.members |> NList.length) |> Span
      DimensionWithoutDefault (domain) -> span // (domain.members |> NList.length) |> Span
   

calculateStart: Int -> Span -> Start -> Start
calculateStart ordinal (Span span) (Start start)  =
         start + span * ordinal
         |> Start

calculateArea: Dimension -> Direction -> Area -> Int -> Area
calculateArea dimension direction area ordinal =
   case direction of
      Horizontal ->
         let
            hSpan: HorizontalSpan
            hSpan = 
               area.horizontalSpan 
               |> horizontalSpanToSpan 
               |> calculateSpan dimension
               |> HorizontalSpan
            hStart: HorizontalStart
            hStart = 
               area.horizontalStart 
               |> horizontalStartToStart
               |> calculateStart ordinal  (hSpan |> horizontalSpanToSpan)
               |> HorizontalStart
            vStart: VerticalStart
            vStart = 
               area.verticalStart 
               |> verticalStartMap startIncrement --Lines.startAdd (Lines.verticalStart area.verticalLine) (Start 1)    
         in
            { 
                     verticalStart = vStart
                 ,   verticalSpan = 1 |> Span |> VerticalSpan
                 ,   horizontalStart = hStart
                 ,   horizontalSpan = hSpan
                 
               --    horizontalLine = HorizontalLine (createLine hStart hSpan)  -- HorizontalLine    { span = horizontalSpan; start = horizontalstart} 
               -- ,  verticalLine = VerticalLine (createLine vStart (Span 1))  --VerticalLine { span =  ; start = verticalStart}
            }
      Vertical ->
         let
            vSpan: VerticalSpan
            vSpan = 
               area.verticalSpan
               |> verticalSpanToSpan
               |> calculateSpan dimension
               |> VerticalSpan
            vStart: VerticalStart
            vStart = 
               area.verticalStart
               |> verticalStartToStart
               |> calculateStart ordinal (vSpan |> verticalSpanToSpan)
               |> VerticalStart
            hStart: HorizontalStart 
            hStart = 
               area.horizontalStart
               |> horizontalStartMap startIncrement
         in
            { 
                     verticalStart = vStart
                 ,   verticalSpan = vSpan
                 ,   horizontalStart = hStart
                 ,   horizontalSpan = 1 |> Span |> HorizontalSpan
                 
               --    horizontalLine = HorizontalLine (createLine hStart (Span 1)) -- { Span = Span 1; Start = horizontalStart}
               -- ,  verticalLine = VerticalLine (createLine vStart vSpan)--{ Span = verticalSpan ; Start = verticalStart }
            }

calculateDefaultArea: Direction -> Depth -> Area -> Area
calculateDefaultArea direction (Depth depth) area =
   -- let
   --    vl: VerticalLine
   --    vl = area.verticalLine
   --    hl: HorizontalLine
   --    hl = area.horizontalLine
   -- in
      case direction of
         Horizontal -> 
            -- let

               {
                     verticalStart = area.verticalStart
                  ,  verticalSpan = VerticalSpan (Span depth)
                  ,  horizontalSpan = HorizontalSpan (Span 1)
                  ,  horizontalStart = 
                     area.horizontalStart  
                     |> horizontalStartMap ((spanStart (\a b -> a + b) (area.horizontalSpan |> horizontalSpanToSpan)) >> Start)
               }
            --    newVl: VerticalLine 
            --    newVl =
            --       vl
            --       |> verticalSetSpan (Span depth)
            --    newHl: HorizontalLine
            --    newHl =
            --       hl
            --       |> horizontalIncrementStartWithSpan 
            --       |> horizontalSetSpan (Span 1)
            -- in
            --    { 
            --          verticalLine = newVl
            --       ,  horizontalLine = newHl
            --    }
         Vertical ->
               {
                     horizontalStart = area.horizontalStart
                  ,  horizontalSpan = HorizontalSpan (Span depth)
                  ,  verticalSpan = VerticalSpan  (Span 1)
                  ,  verticalStart = 
                     area.verticalStart  
                     |> verticalStartMap ((spanStart (\a b -> a + b) (area.verticalSpan |> verticalSpanToSpan)) >> Start)
               }
            -- let
            --    newHl: HorizontalLine 
            --    newHl =
            --       hl
            --       |> horizontalSetSpan (Span depth)
            --    newVl: VerticalLine
            --    newVl =
            --       vl
            --       |> verticalIncrementStartWithSpan 
            --       |> verticalSetSpan (Span 1)
            -- in
            --    { 
            --          verticalLine = newVl
            --       ,  horizontalLine = newHl
            --    }

fromDimension: Direction -> Depth -> Area -> Dimension -> List Member -> (NList DimensionHeader, Maybe DimensionHeader )
fromDimension direction depth area dimension parentMembers =
                
      let
         dms: NList DomainMember
         dms = dimensionMembers dimension
         dm: Maybe DefaultMember
         dm = memberDefault dimension
         memberHeaders: NList DimensionHeader
         memberHeaders = 
            dms
            |> NList.mapi
               (\ i (DomainMember m) ->  
                     m
                     |> createDimensionHeader (calculateArea dimension direction area i) parentMembers) 
               
         -- todo: gör om till lista som inte kan vara tom
         getLastArea: NList DimensionHeader -> Area 
         getLastArea headers = 
            headers 
            |> NList.last
            |> (\ (DimensionHeader item) -> item.area)

         defaultMemberHeader: Maybe DimensionHeader
         defaultMemberHeader =
            dm
            |> Maybe.map (\ (DefaultMember md) ->  
               md 
               |> createDimensionHeader (calculateDefaultArea direction depth (getLastArea memberHeaders)) parentMembers)
      in
         (memberHeaders, defaultMemberHeader)


-- type MemberHeader = MemberHeader DimensionHeader
-- type DefaultHeader = DefaultHeader DimensionHeader
type TableHeader  = 
   MemberHeader (NList DimensionHeader)
   | TotalHeader (NList DimensionHeader)

tableHeaderAsMembers: TableHeader -> NList Member
tableHeaderAsMembers acc =
   let 
      headers: NList DimensionHeader
      headers =
         case acc of
            MemberHeader hs -> hs
            TotalHeader hs -> hs
   in 
     headers
     |> (\ h -> h.head)
     |> (\ (DimensionHeader item)-> item.member)


-- accumulatedHeaders: TableHeader -> List DimensionHeader
-- accumulatedHeaders acc =
--    case acc of
--       MemberHeader headers -> headers |> NList.toList
--       TotalHeader headers -> headers |> NList.toList
      


dimensionAsTableHeader: Direction -> Depth -> Dimension -> NList DimensionHeader -> NList TableHeader
dimensionAsTableHeader direction depth dimension headers =
   let 

      header: DimensionHeader
      header = headers.head

      columnHeaders: DimensionHeader -> (NList DimensionHeader, Maybe DimensionHeader)
      columnHeaders (DimensionHeader item)  = 
         let
            area: Area 
            area = item.area
            members: List Member
            members = item.member |> NList.toList
         in 
            fromDimension direction depth area dimension members

      mapHeaders: Int -> NList a -> List a
      mapHeaders i hs =
         if i == 0 then
            hs |> NList.toList
         else 
            []

      result: (NList DimensionHeader, Maybe DimensionHeader)
      result = columnHeaders header
      v1: NList TableHeader  
      v1 =  (first result)
            |> NList.mapi (\ i m -> MemberHeader (NList.create2 m (mapHeaders i headers))) 
      v2: List TableHeader 
      v2 = 
         (second result)
         |> Lists.maybeAsList
         |> List.map (NList.create >> TotalHeader)
   in
      NList.addList v1 v2

addDimensionToTableHeader: Direction -> Depth -> Dimension -> TableHeader -> List TableHeader
addDimensionToTableHeader direction depth dimension acc =
   let
      result: NList TableHeader
      result =
         case acc of
            TotalHeader _ -> NList.create acc
            MemberHeader dimensionHeaders -> dimensionAsTableHeader direction depth dimension dimensionHeaders
   in
      result |> NList.toList

addDimensionToTableHeaders: Direction -> Depth -> Span -> Dimension -> List TableHeader -> List TableHeader
addDimensionToTableHeaders direction depth span dimension acc =
   let 
      initArea: Direction -> Span -> Area
      initArea d totalSpan =
            case d of
               Horizontal ->
                  {
                        verticalSpan =  0 |> Span |> VerticalSpan
                     ,  verticalStart = 0 |> Start |> VerticalStart
                     ,  horizontalSpan = totalSpan |> HorizontalSpan
                     ,  horizontalStart = 1 |> Start |> HorizontalStart
                  }
                  -- { verticalLine = VerticalLine { span = Span 0, start = Start 0},  horizontalLine = HorizontalLine {span = totalSpan, start = Start 1}}
               Vertical ->
                  {
                        verticalSpan = totalSpan |> VerticalSpan
                     ,  verticalStart =  1 |> Start |> VerticalStart
                     ,  horizontalSpan = 0 |> Span |> HorizontalSpan
                     ,  horizontalStart =  0 |> Start|> HorizontalStart
                  }
                  -- { verticalLine = VerticalLine  { span = totalSpan, start = Start 1}, horizontalLine = HorizontalLine {span = Span 0, start = Start 0}}
   in
      case acc of
         [] -> 
            let 
               area: Area 
               area = initArea direction span
               mt: (NList DimensionHeader, Maybe DimensionHeader)
               mt = fromDimension direction depth area dimension []
               v1: NList TableHeader  
               v1 =  (first mt)
                     |> NList.map (NList.create >> MemberHeader)
               v2: List TableHeader 
               v2 = 
                  (second mt)
                  |> Lists.maybeAsList
                  |> List.map (NList.create >> TotalHeader)
            in
               NList.addList v1 v2
               |> NList.toList
         _ -> acc |> Lists.collect (addDimensionToTableHeader direction depth dimension)

calculateSpanForDimensions: List Dimension -> Span
calculateSpanForDimensions dimensions =
   let
      recCalculateSpan: List Dimension -> Int
      recCalculateSpan d =
         case d of
            [] -> 1
            head :: tail ->
               case head of
                  DimensionWithDefault (_,m) ->  (NList.length m.members) * (recCalculateSpan tail) + 1 
                  DimensionWithoutDefault (m)-> (NList.length m.members) * (recCalculateSpan tail)
   in
      dimensions
      |> recCalculateSpan 
      |> Span
                  

calculateTableHeaders: Direction -> (List Dimension) -> List TableHeader
calculateTableHeaders direction dimensions =
   let
      calculateNextDepth: Depth -> Depth
      calculateNextDepth (Depth d) = (d - 1) |> Depth
      recFold: Span -> Depth -> List TableHeader -> List Dimension ->  List TableHeader 
      recFold span depth acc dims  =
         case dims of
            [] -> acc
            head :: tail ->   
               let
                  nextDepth: Depth 
                  nextDepth = calculateNextDepth depth
                  state: List TableHeader 
                  state = addDimensionToTableHeaders direction depth span head acc 
               in
                  recFold span nextDepth state tail 
   in
      let
         totalSpanForDimensions : Span
         totalSpanForDimensions = calculateSpanForDimensions dimensions
      in
         dimensions
         |> recFold totalSpanForDimensions (dimensions |> List.length |> Depth) []  

type CubeColumn = CubeColumn (NList Member)


type alias CubeColumnHeader =
   {
         isTotal: Bool
      ,  area: Area
      ,  member: Member
   }

tableHeaderToDimensionColumnHeader: TableHeader -> List CubeColumnHeader
tableHeaderToDimensionColumnHeader tableHeader =
   let   
      createDimensionColumnHeader: Bool -> DimensionHeader -> CubeColumnHeader
      createDimensionColumnHeader isTotal (DimensionHeader d) =
         {
               isTotal = isTotal
            ,  area = d.area
            ,  member = d.member.head  
         }
   in
      let
         ch: NList CubeColumnHeader
         ch =
            case tableHeader of
               MemberHeader h -> h |> NList.map (createDimensionColumnHeader False)
               TotalHeader h -> h |> NList.map (createDimensionColumnHeader True)
      in
   
         ch |> NList.toList


dimensionColumns: List TableHeader -> List CubeColumn
dimensionColumns headers = 
  headers 
  |> List.map (tableHeaderAsMembers >> CubeColumn)


dimensionColumnHeaders: List TableHeader -> List CubeColumnHeader 
dimensionColumnHeaders headers =
   headers
   |> Lists.collect tableHeaderToDimensionColumnHeader

type CubeRowOffset = CubeRowOffset Offset

type alias CubeColumns = 
   {
         offset: CubeRowOffset
      ,  columns: List CubeColumn 
      ,  headers: List CubeColumnHeader 
   }


calculateCubeColumns:  Direction ->  List Dimension -> CubeColumns  
calculateCubeColumns direction dimensions =
   let
      tableHeaders: List TableHeader
      tableHeaders = calculateTableHeaders direction dimensions
      offset : Offset
      offset =
         case direction of
            Horizontal -> 
               dimensions 
               |> List.length 
               |> Start 
               |> VerticalStart
               |> addVerticalStartToOffset Area.emptyOffset  
            Vertical -> 
               dimensions 
               |> List.length 
               |> Start 
               |> HorizontalStart 
               |> addHorizontalStartToOffset Area.emptyOffset  
   in
      {
            columns = dimensionColumns tableHeaders
         ,  headers = dimensionColumnHeaders tableHeaders
         ,  offset = offset |>  CubeRowOffset
      }


--------------------------------- View -----------------------------------



gridSizeAttribute:  String -> String -> Int -> String -> Attribute msg
gridSizeAttribute s1 s2 i s3 =
   i 
   |> String.fromInt
   |> (\s -> s2 ++ s ++ s3)
   |> style s1


type GridColumns = GridColumns Int
columnsInt: GridColumns -> Int
columnsInt (GridColumns columns) = columns

type GridRows = GridRows Int
rowsInt: GridRows -> Int
rowsInt (GridRows rows) = rows

grid: GridRows -> GridColumns -> List (Attribute msg) 
grid (GridRows rows) (GridColumns cols)=
    let 
      
        attrdisplay: Attribute msg
        attrdisplay = style "display" "grid"  
      -- Horizontal span
        attrColumns: Attribute msg
        attrColumns = gridSizeAttribute "grid-template-columns" "repeat(" cols ")"
      -- vertical span
        attrRows: Attribute msg
        attrRows = gridSizeAttribute "grid-template-rows" "repeat(" rows ", minmax(50px,100px))"

    in
     [ attrdisplay , attrColumns]



areaToAttribute: Area -> List (Attribute msg)
areaToAttribute area =
    let 
        row: String 
        row = area.verticalStart |> verticalStartToInt |> String.fromInt |> (\s -> s )
        col: String 
        col = area.horizontalStart |> horizontalStartToInt |> String.fromInt |> (\s -> " / " ++ s)
        colSpan: String 
        colSpan = area.horizontalSpan |> horizontalSpanToInt |> String.fromInt |> (\s ->" / span " ++ s )
        rowSpan: String 
        rowSpan = area.verticalSpan |> verticalSpanToInt |> String.fromInt |> (\s ->" / span " ++ s)
        areaAttribute: Attribute msg   
        areaAttribute = style "grid-area" (row ++ col ++ rowSpan ++ colSpan )
    in
         [ areaAttribute,style "border" "black solid 1px" ]

gridItem: Offset -> Area -> String -> Html Msg
gridItem offset area t = 
    let
         areaWithOffset: Area 
         areaWithOffset = area |> offsetArea offset
    in
      div (areaToAttribute areaWithOffset) [ text t ]

gridColumnItem: CubeColumnOffset -> Area -> String -> Html Msg
gridColumnItem (CubeColumnOffset offset) =
   gridItem offset


gridIndentedRowItem: CubeRowOffset -> Area -> String -> Html Msg
gridIndentedRowItem (CubeRowOffset offset) area =
   let 
         newArea: Area
         newArea =
            area
            |> Area.setHorizontalSpan (Span 1)  
            |> Area.setHorizontalStart (Start 1)
   in
      gridItem offset newArea

viewCube: Direction -> HyperCube -> List DimensionalKoncept  -> Html Msg
viewCube direction hyperCube koncepts =
    let 

         dimensions: List Dimension 
         dimensions = 
            hyperCube.dimensions 
            |> NList.map hyperDimensionAsDimension
            |> NList.toList

         span: Span 
         span = dimensions |> calculateSpanForDimensions 

         -- tableHeaders: List TableHeader
         -- tableHeaders = 
         --    dimensions 
         --    |> calculateTableHeaders direction
         cubeRows: CubeRows
         cubeRows = calculateIndentedCubeRows koncepts


         cubeColumns: CubeColumns
         cubeColumns = 
            dimensions
            |> calculateCubeColumns direction
       
         columns: List (Html Msg) 
         columns = 
            cubeColumns.headers 
            |> List.map (\header -> gridColumnItem cubeRows.offset header.area header.member.name)

         rowHeaders: List (Html Msg)
         rowHeaders =
            cubeRows.rows
            |> List.map (\rowHeader -> gridIndentedRowItem cubeColumns.offset rowHeader.area (konceptRowItemName rowHeader.item))



         gridRows: Direction -> Span -> List CubeColumn -> GridRows
         gridRows d (Span s) cols  =
            case d of
               Horizontal ->
                  GridRows s
               Vertical ->
                  GridRows (List.length cols)
        
         gridColumns: Direction -> Span -> List CubeColumn -> GridColumns
         gridColumns d (Span s) cols =
            case d of
               Horizontal ->
                  GridColumns (List.length cols)
               Vertical ->
                  GridColumns s
        
    in
 
        div (grid (gridRows direction span cubeColumns.columns) (gridColumns direction span cubeColumns.columns)) (columns ++ rowHeaders)


-- tableHeaderView: List TableColumn -> Html Msg
-- tableHeaderView columns = 
--    let -> 
--       tableColumAsHtml: TableColumns -> List (Html Msg)
--       tableColumnAsHtml c =
--          c.    

-- let calculateColumnsAndHeaders: Direction -> List Dimension -> (TableColumns, 

   
   --      let (SimpleHeader (Header s)) = headers.Head
   -- let simples,total = Header.fromDimension direction depth s.Area dimension
   -- // match header with
   -- let notWritableHeaders = headers |> List.map (fun (SimpleHeader (Header item)) -> { item with Write = false } |> Header |> SimpleHeader)
   -- let mapHeaders i headers= 
   --    if i = 0 then 
   --       headers
   --    else notWritableHeaders 
      
   -- // | Simple s -> 
   -- let v1 = simples |> List.mapi (fun i simple -> Simple (SimpleHeader simple :: (mapHeaders i headers ))) 
   -- let v2 = total |> Option.toList |> List.map (fun t -> Total (TotalHeader t,notWritableHeaders))
   -- v1 @ v2

-- fromDimension: Area.Direction -> Area.Depth -> Dimensional.Dimension -> (DimensionHeader List, Maybe DimensionHeader)
-- fromDimension direction depth area dimension =
--    let
--       members: List DomainMember 
--       members = Dimension.members dimension
--       defaultMember: Maybe DefaultMember
--       defaultMember = Dimension.defaultMember dimension

--       calcSpan: Maybe DefaultMember ->  Span -> Span
--       calcSpan span defaultMember =
--          case defaultMember of
--             Just _ -> spanAdd (-1) span
--             Nothing -> span
--          / members.length

--       calcStart: Int -> Start -> Span -> Start
--       calcStart ordinal (Start start) (Span span) =
--          start + span * ordinal
--          |> Start






   