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


attrSelected: String -> Bool -> List (Attribute msg)
attrSelected c b =
   if b then [ class c ] else []

attrColumnHeaderSelected: Bool -> List (Attribute msg)
attrColumnHeaderSelected  = attrSelected "grid-cell-selected" 
   
attrRowHeaderSelected: Bool -> List (Attribute msg)
attrRowHeaderSelected  = attrSelected "grid-cell-selected"
  
attrCellPathToSelection: Bool -> List (Attribute msg)
attrCellPathToSelection = attrSelected "grid-cell-selected"
    
attrSelectedCell: Bool -> List (Attribute msg)
attrSelectedCell = attrSelected "grid-cell-selected"

attrMemberCell: Bool -> List (Attribute msg)
attrMemberCell = attrSelected "grid-cell-member"

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


columnHeaderCell: CubeColumnOffset -> CubeColumnHeader -> Html Msg
columnHeaderCell (CubeColumnOffset offset) colunmHeader =
   
   colunmHeader.area
   |> offsetArea offset
   |> attributeGridArea
   |> List.append attrCell
   |> List.append attrBox
   |> List.append (attrColumnHeaderSelected colunmHeader.isSelected)
   |> textCell colunmHeader.member.name


rowHeaderCellIndented: CubeRowOffset -> Maybe ValueKoncept -> KonceptRow -> Html Msg
rowHeaderCellIndented (CubeRowOffset offset) selection rowHeader =
   let    
        
         newArea: Area
         newArea =
            rowHeader.area
            |> Area.setHorizontalSpan (Span 1)  
            |> Area.setHorizontalStart (Start 1)
            |> offsetArea offset
   in 
      let 
         selected =         
            case selection of
               Just vk ->
                  case konceptRowFactor rowHeader.item of
                     Just factor -> factor == vk.factor
                     Nothing -> False
               Nothing -> False
      in
         attrCell
         |> addAttr attrBox
         |> addAttr (attrIndentHorizontalStart newArea.horizontalStart)
         |> addAttr (attributeGridArea newArea)
         |> addAttr (attrRowHeaderSelected selected)
         |> textCell (konceptRowItemName rowHeader.item)   

-- selectionForCells: Maybe (ValueKoncept, List Member) -> Maybe (ValueKoncept, Member)
-- selectionForCells selection =
--    case selection of
--       Just (vk,members) ->
--          case members of
--            [] -> Nothing
--            head :: tail -> Just (vk, members)
--       Nothing -> Nothing

factorFromMemberList: List Member -> Int
factorFromMemberList members =
   case members of
      [] -> 1
      head :: tail ->
          head.factor
          |> factorToInt
          |> (\i -> i * factorFromMemberList tail)

factorFromValue: ValueKoncept -> Int -> Int
factorFromValue vk mv =
   vk.factor
   |> factorToInt
   |> (\i -> i * mv)

factorFromSelection: ValueKoncept -> List Member -> Int
factorFromSelection vk =
   factorFromMemberList >> (factorFromValue vk)

-- TODO: Add
trySelectCell: Bool -> Maybe (ValueKoncept, List Member) -> KonceptRow -> CubeColumn -> List (Attribute Msg)
trySelectCell skip selection row (CubeColumn cellMembers) =
   if skip then []
   else
      let
         result =
            case selection of
               Nothing -> []
               Just (vk,members) -> 
                  case tryGetValueKoncept row.item of
                     Just vkCell -> 
                        let
                           productSelection = factorFromSelection vk members
                           productCell = factorFromSelection vkCell (cellMembers |> NList.toList)
                        in

                        attrSelectedCell (productSelection ==  productCell) ---(factor == vk.factor && members.head.factor == member.factor)
                     Nothing -> []
      in
         result

trySelectMemberCell: Bool -> Maybe (ValueKoncept, List Member) -> KonceptRow -> CubeColumn -> List (Attribute Msg)
trySelectMemberCell skip selection row (CubeColumn cellMembers) =
   if skip then []
   else
      let 
         result =
            case selection of
               Nothing -> []
               Just (vk,members) -> 
                  case tryGetValueKoncept row.item of
                     Just vkCell -> 
                        let
                           productSelection = factorFromMemberList members
                           productCell = factorFromMemberList (cellMembers |> NList.toList)
                        in

                        attrMemberCell ((productSelection ==  productCell) || vkCell.factor == vk.factor)  ---(factor == vk.factor && members.head.factor == member.factor)
                     Nothing -> []
      in
         result

calculateIfCellIsSelected: Maybe (ValueKoncept, List Member) -> KonceptRow -> CubeColumn -> Bool
calculateIfCellIsSelected selection konceptRow (CubeColumn cellMembers) =
   case selection of
      Nothing -> False
      Just (vk,members) -> 
          case tryGetValueKoncept konceptRow.item of
            Just vkCell -> 
               let
                   productSelection = factorFromSelection vk members
                   productCell = factorFromSelection vkCell (cellMembers |> NList.toList)
               in
               productSelection == productCell ---(factor == vk.factor && members.head.factor == member.factor)
            Nothing -> False

cell: Direction -> Area -> Maybe (ValueKoncept, List Member) -> Int -> CubeColumn -> Int -> KonceptRow -> (Bool,List (Html Msg)) -> (Bool,List (Html Msg))
cell direction area selection columnIndex column rowIndex row state  =
   let 
       
      skipSelection = first state
      newSelectionState = 
         if (first state) then True
         else calculateIfCellIsSelected selection row column

      newCell: Html Msg
      newCell =
         case direction of
            Horizontal -> 
               area
               |> Area.addHorizontalStart (columnIndex |> Start |> HorizontalStart)
               |> Area.addVerticalStart (rowIndex |> Start |> VerticalStart)
               |> attributeGridArea 
               |> addAttr attrCell
               |> addAttr attrBox
               |> addAttr (trySelectCell skipSelection selection row column)  
               |> addAttr (trySelectMemberCell skipSelection selection row column)  
               |> attrOnClickCell column row                      
               |> textCell ""

            Vertical ->
               area
               |> Area.addVerticalStart (columnIndex |> Start |> VerticalStart)
               |> Area.addHorizontalStart (rowIndex |> Start |> HorizontalStart)
               |> attributeGridArea 
               |> addAttr attrCell
               |> addAttr attrBox
               |> addAttr (trySelectCell skipSelection selection row column)  
               |> addAttr (trySelectMemberCell skipSelection selection row column)  
               |> attrOnClickCell column row
               |> textCell ""
   in

      (newSelectionState, [ newCell ] ++ (second state))


gridCells: Direction -> Maybe (ValueKoncept, List Member) -> CubeColumns -> CubeRows -> List (Html Msg)
gridCells direction selection cubeColumns cubeRows =
   let 
      area: Area 
      area = 
         Area.emptyArea 
         -- add offset for columns and rows
         |> offsetArea (cubeRowOffsetToOffset cubeColumns.offset) 
         |> offsetArea (cubeColumnOffsetToOffset cubeRows.offset)
         |> Area.addVerticalSpan Area.oneVerticalSpan
         |> Area.addHorizontalSpan Area.oneHorizontalSpan
   in  
      let    
         cells: List KonceptRow -> Int -> CubeColumn -> (Bool,List (List (Html Msg))) -> (Bool,List (List (Html Msg)))
         cells rows columnIndex cubeColumn acc =
            let 
               selected = first acc
               newState: (Bool, List (Html Msg))
               newState = 
                   -- fold over rows (KonceptRow)
                   rows
                   |> Lists.foldi (\i state row-> cell direction area selection columnIndex cubeColumn (i + 1) row state) (selected,[])
              
            in
               (first newState, [ second newState ] ++ (second acc))
              
      in
         --- fold over columns
         cubeColumns.columns
         |> Lists.foldi (\i state col -> (cells cubeRows.rows) (i + 1) col state) (False,[])
         |> (\(_,result) -> result)
         |> List.concat

-- gridCells: Direction -> Maybe (ValueKoncept, List Member) -> CubeColumns -> CubeRows -> List (Html Msg)
-- gridCells direction selection columns rows =
--    let 
--       area: Area 
--       area = 
--          Area.emptyArea 
--          |> offsetArea (cubeRowOffsetToOffset columns.offset) 
--          |> offsetArea (cubeColumnOffsetToOffset rows.offset)
--          |> Area.addVerticalSpan Area.oneVerticalSpan
--          |> Area.addHorizontalSpan Area.oneHorizontalSpan

--    in  
--       let    
--          cellRows: List KonceptRow -> Int -> CubeColumn -> List (Html Msg) 
--          cellRows cubeRows columnIndex cubeColumn =
--             let 
--                cell: Int -> KonceptRow ->  (Html Msg)
--                cell rowIndex row    =
--                   case direction of
--                      Horizontal -> 
--                         area
--                         |> Area.addHorizontalStart (columnIndex |> Start |> HorizontalStart)
--                         |> Area.addVerticalStart (rowIndex |> Start |> VerticalStart)
--                         |> attributeGridArea 
--                         |> addAttr attrCell
--                         |> addAttr attrBox
--                         |> addAttr (trySelectCell selection row cubeColumn)  
--                         |> attrOnClickCell cubeColumn row                      
--                         |> textCell ""

--                      Vertical ->
--                         area
--                         |> Area.addVerticalStart (columnIndex |> Start |> VerticalStart)
--                         |> Area.addHorizontalStart (rowIndex |> Start |> HorizontalStart)
--                         |> attributeGridArea 
--                         |> addAttr attrCell
--                         |> addAttr attrBox
--                         |> addAttr (trySelectCell selection row cubeColumn)  
--                         |> attrOnClickCell cubeColumn row
--                         |> textCell ""
--             in
--                cubeRows
--                |> Lists.mapi (\i row-> cell (i + 1) row)
--       in
--          columns.columns
--          |> Lists.mapi (\i col -> (cellRows rows.rows) (i + 1) col)
--          |> List.concat

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
            |> List.map (\header -> columnHeaderCell cubeRows.offset header)

         cells: List (Html Msg) 
         cells = 
            gridCells direction selection cubeColumns cubeRows

         rowHeaders: List (Html Msg)
         rowHeaders =
               cubeRows.rows
               |> List.map (\rowHeader -> rowHeaderCellIndented cubeColumns.offset (first selectedFilter) rowHeader)

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