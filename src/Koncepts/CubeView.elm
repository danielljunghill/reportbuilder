module Koncepts.CubeView exposing (..)
import Koncepts.CubeKoncept exposing (..)
import Koncepts.CubeDimension exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Area as Area
import Koncepts.Model exposing (..)
import Lists as Lists
import NList exposing (..)
import Tuple exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Msg exposing (..)
import Model exposing (..)

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
     [  attrdisplay , attrColumns ]



attributeGridArea: Area -> List (Attribute msg)
attributeGridArea  area =
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
         [ areaAttribute ]


attrBox: List (Attribute msg)
attrBox = [ style "border" "black 1px solid" ]


attrLeftIndent: Int -> List (Attribute msg)
attrLeftIndent indent =
   if indent > 0 && indent < 11 then
      [ class ("rind" ++ String.fromInt indent) ]
   else
      []

attrIndentHorizontalStart: HorizontalStart -> List (Attribute msg)
attrIndentHorizontalStart (HorizontalStart (Start start)) = attrLeftIndent start
   
addAttr: List (Attribute msg) -> List (Attribute msg) -> List (Attribute msg) 
addAttr a1 a2 =
   List.append a1 a2

attrCell: List (Attribute msg)
attrCell  = 
    [ class "grid-cell" ] 


textCell: String -> List (Attribute Msg) -> Html Msg
textCell s attr  =
   div attr [ text s ]

attrOnClickCell: CubeColumn -> KonceptRow  -> List (Attribute Msg) -> List (Attribute Msg)
attrOnClickCell (CubeColumn members) row attr = 
   case row.item of
      AbstractRow ak ->
         attr 
      ValueRow vk ->
         let
            event: Attribute Msg 
            event = 
               (vk,members |> NList.toList) 
               |> SelectedCell 
               |> Select 
               |> onClick
         in
            [event] ++ attr


      

columnHeaderCell: CubeColumnOffset -> Area -> String -> Html Msg
columnHeaderCell (CubeColumnOffset offset) area s =
   area
   |> offsetArea offset
   |> attributeGridArea
   |> List.append attrCell
   |> List.append attrBox
   |> textCell s

rowHeaderCellIndented: CubeRowOffset -> Area -> String -> Html Msg
rowHeaderCellIndented (CubeRowOffset offset) area s =
   let        
         newArea: Area
         newArea =
            area
            |> Area.setHorizontalSpan (Span 1)  
            |> Area.setHorizontalStart (Start 1)
            |> offsetArea offset
   in

     
      attrCell
      |> addAttr attrBox
      |> addAttr (attrIndentHorizontalStart area.horizontalStart)
      |> addAttr (attributeGridArea newArea)
      |> textCell s



gridCells: Direction -> CubeColumns -> CubeRows -> List (Html Msg)
gridCells direction columns rows =
   let 
      area: Area 
      area = 
         Area.emptyArea 
         |> offsetArea (cubeRowOffsetToOffset columns.offset) 
         |> offsetArea (cubeColumnOffsetToOffset rows.offset)
         |> Area.addVerticalSpan Area.oneVerticalSpan
         |> Area.addHorizontalSpan Area.oneHorizontalSpan
         
   in  
      let    
         cellRows: List KonceptRow -> Int -> CubeColumn -> List (Html Msg) 
         cellRows cubeRows columnIndex cubeColumn =
            let 
               cell: Int -> KonceptRow ->  (Html Msg)
               cell rowIndex row    =
                  case direction of
                     Horizontal -> 
                        area
                        |> Area.addHorizontalStart (columnIndex |> Start |> HorizontalStart)
                        |> Area.addVerticalStart (rowIndex |> Start |> VerticalStart)
                        |> attributeGridArea 
                        |> addAttr attrCell
                        |> addAttr attrBox
                        |> attrOnClickCell cubeColumn row
                        |> textCell ""

                     Vertical ->
                        area
                        |> Area.addVerticalStart (columnIndex |> Start |> VerticalStart)
                        |> Area.addHorizontalStart (rowIndex |> Start |> HorizontalStart)
                        |> attributeGridArea 
                        |> addAttr attrCell
                        |> addAttr attrBox
                        |> attrOnClickCell cubeColumn row
                        |> textCell ""
            in
               cubeRows
               |> Lists.mapi (\i row-> cell (i + 1) row)
      in
         columns.columns
         |> Lists.mapi (\i col -> (cellRows rows.rows) (i + 1) col)
         |> List.concat

viewCube: Direction -> HyperCube -> List DimensionalKoncept -> Maybe (ValueKoncept, List Member) -> Html Msg
viewCube direction hyperCube koncepts selection =
    let 

         selectedFilter:(Maybe ValueKoncept, List Member)
         selectedFilter =
            case selection of
                Just (vk,m) -> (Just vk, m)
                Nothing -> (Nothing, [])

         dimensions: List Dimension 
         dimensions = 
            hyperCube.dimensions 
            |> NList.map hyperDimensionAsDimension
            |> NList.toList

         span: Span 
         span = dimensions |> calculateSpanForDimensions 

         cubeRows: CubeRows
         cubeRows = calculateIndentedCubeRows koncepts  

         cubeColumns: CubeColumns
         cubeColumns = 
            dimensions
            |> calculateCubeColumns direction (second selectedFilter)
       
         columns: List (Html Msg) 
         columns = 
            cubeColumns.headers 
            |> List.map (\header -> columnHeaderCell cubeRows.offset header.area header.member.name)

         cells: List (Html Msg) 
         cells = 
            gridCells direction cubeColumns cubeRows

         rowHeaders: List (Html Msg)
         rowHeaders =
               cubeRows.rows
               |> List.map (\rowHeader -> rowHeaderCellIndented cubeColumns.offset rowHeader.area (konceptRowItemName rowHeader.item))

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
        div (grid (gridRows direction span cubeColumns.columns) (gridColumns direction span cubeColumns.columns)) (columns ++ rowHeaders ++ cells)