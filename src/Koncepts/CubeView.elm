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
import Events.Custom exposing (..)



attrEventSelectCell: NList Factor -> KonceptRow -> List (Attribute Msg) -> List (Attribute Msg)
attrEventSelectCell factors row attr = 
   case row.item of
      AbstractRow ak ->
         attr 
      ValueRow vk ->
         let
            event: Attribute Msg 
            event = 
               Msg.Select factors
               |> onClick
         in
            [event] ++ attr

attrEventEditCell: NList Factor  ->  KonceptRow  -> List (Attribute Msg) -> List (Attribute Msg)
attrEventEditCell factors row attr = 
   case row.item of
      AbstractRow ak ->
         attr 
      ValueRow vk ->
         let
            event: Attribute Msg 
            event = 
               Msg.Edit (factors,Content "")
               |> onDoubleClick
         in
            [event] ++ attr

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
   
textCell: String -> List (Attribute Msg) -> Html Msg
textCell s attr  =
   div attr [ text s ]

inputCell: String -> List (Attribute Msg) -> Html Msg
inputCell s attr  =
    input ([ value s, onClickStopPropagation DoNothing ] ++ attr) []
   
    

columnHeaderCell: CubeColumnOffset -> CubeColumnHeader -> Html Msg
columnHeaderCell (CubeColumnOffset offset) colunmHeader =
   
   colunmHeader.area
   |> offsetArea offset
   |> attributeGridArea
   |> List.append attrCell
   |> List.append attrBox
   |> List.append (attrColumnHeaderSelected colunmHeader.isSelected)
   |> textCell colunmHeader.member.name

attrRowHeaderAbstract: KonceptRow -> List (Attribute msg)
attrRowHeaderAbstract rowHeader = 
   case tryGetValueKoncept rowHeader.item of
      Nothing -> [ class "abstract-header" ]
      Just _ -> []


rowHeaderCellIndented: CubeRowOffset -> List Factor -> KonceptRow -> Html Msg
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
            case konceptRowFactor rowHeader.item of
               Just factor -> selection |> Lists.contains factor
               Nothing -> False

      in
         attrCell
         |> addAttr attrBox
         |> addAttr (attrIndentHorizontalStart rowHeader.area.horizontalStart)
         |> addAttr (attributeGridArea newArea)
         |> addAttr (attrRowHeaderSelected selected)
         |> addAttr (attrRowHeaderAbstract rowHeader)
         |> textCell (konceptRowItemName rowHeader.item)   
      

membersToFactors: NList Member -> NList Factor
membersToFactors = NList.map (\m -> m.factor)

multiplyFactors: NList Factor -> Factor
multiplyFactors factors = factors |> NList.fold multiplyFactor (Factor 1) 


-- calculate factor for selection
factorsFromSelection: Selection -> NList Factor
factorsFromSelection selection =
      case selection of
         Editing factors -> factors            
         Selecting factors ->  factors  
  
-- calculate if cell is selected/edited            
isSelected: Selection -> NList Factor -> Bool
isSelected selection factorsCell =
   let 
      productCell = factorsCell |> multiplyFactors
   in 
      selection
      |> factorsFromSelection 
      |> multiplyFactors 
      |> (\factors -> factors == productCell) 



type SelectionFactor = SelectionFactor Factor
type MembersFactor = MembersFactor Factor
type ValueFactor = ValueFactor Factor

createSelectionFactor:  Selection -> SelectionFactor
createSelectionFactor  =
      
       factorsFromSelection  
      >> multiplyFactors
      >> SelectionFactor

selectionFactorToInt (SelectionFactor (Factor factor)) = factor

createMembersFactor: CubeColumn -> MembersFactor
createMembersFactor (CubeColumn members) =
   members
   |> membersToFactors
   |> multiplyFactors
   |> MembersFactor

membersFactorToInt (MembersFactor (Factor factor)) = factor

createValueFactor: ValueKoncept -> ValueFactor
createValueFactor koncept =
   koncept.factor |> ValueFactor

valueFactorToInt (ValueFactor (Factor factor)) = factor


memberInSelection (MembersFactor (Factor member)) (SelectionFactor (Factor selection)) =
   (modBy member selection) == 0

valueInSelection (ValueFactor (Factor value)) (SelectionFactor (Factor selection)) =
    (modBy value selection) == 0 

valueAndMemberInSelection (MembersFactor (Factor member)) (ValueFactor (Factor value)) (SelectionFactor (Factor selection)) =
   member * value == selection


cellFactors: KonceptRow -> CubeColumn -> NList Factor 
cellFactors konceptRow (CubeColumn members) =
   case tryGetValueKoncept konceptRow.item of
      Just valueKoncept ->
         NList.addFirst valueKoncept.factor (members |> membersToFactors)
      Nothing -> members |> membersToFactors


type RowIndex = RowIndex Int
type ColumnIndex = ColumnIndex Int

cellAreaAttributes: Direction -> Area -> ColumnIndex -> RowIndex -> List (Attribute Msg)
cellAreaAttributes direction area (ColumnIndex colIndex) (RowIndex rowIndex) =
   case direction of
      Horizontal ->
         area
         |> Area.addHorizontalStart (colIndex |> Start |> HorizontalStart)
         |> Area.addVerticalStart (rowIndex |> Start |> VerticalStart)
         |> attributeGridArea 

      Vertical ->
         area
         |> Area.addVerticalStart (colIndex |> Start |> VerticalStart)
         |> Area.addHorizontalStart (rowIndex |> Start |> HorizontalStart)
         |> attributeGridArea 

attrRowHeaderSelected  = attrSelected "grid-cell-selected"


attrSelectedCell = attrSelected "grid-cell-selected-1"
attrAssociatedCell = attrSelected "grid-cell-member"


-- editHtml: Bool -> selectionFactor -> KonceptRow -> MembersFactor -> List (Attribute Msg) -> (Bool, List (Html Msg))
-- editHtml skip selectionFactor konceptRow membersFactor =
--    if skip then (skip, [ textCell "1" attributes ])
--    else
--       case tryGetValueKoncept konceptRow.item of
--          Just koncept ->
--             -- cell is selected
--             if valueAndMemberInSelection  membersFactor (ValueFactor koncept.factor)  selectionFactor then
--                (True,attrSelectedCell True)  
--             else
--                (False, attrAssociatedCell ((valueInSelection (ValueFactor koncept.factor) selectionFactor) || (memberInSelection membersFactor selectionFactor)))

--          Nothing ->
--             (False,attrAssociatedCell (memberInSelection  membersFactor selectionFactor))
         
type CellCreator = CellCreator (String -> List (Attribute Msg) -> Html Msg)
selectionHtml: Bool -> SelectionFactor-> KonceptRow -> MembersFactor -> List (Attribute Msg) -> CellCreator -> (Bool, (Html Msg))
selectionHtml skip selectionFactor konceptRow membersFactor attributes (CellCreator createCell)=
   if skip then (skip,  textCell "" attributes)
   else
      case tryGetValueKoncept konceptRow.item of
         Just koncept ->
            -- cell is selected
            if valueAndMemberInSelection  membersFactor (ValueFactor koncept.factor)  selectionFactor then
               (True,
                  attributes
                  |> addAttr (attrSelectedCell True)
                  |> createCell "")
                 
            else
               let
                  isAssociated = (valueInSelection (ValueFactor koncept.factor) selectionFactor) || (memberInSelection membersFactor selectionFactor)
               in 
               (False, 
                  attributes
                  |> addAttr (attrAssociatedCell isAssociated)
                  |>  textCell "")

         Nothing ->
            (False,
               attributes
               |> addAttr ( attrAssociatedCell (memberInSelection  membersFactor selectionFactor))
               |> textCell "" )
         
cellHtml: Bool -> Maybe Selection -> KonceptRow -> CubeColumn -> List (Attribute Msg) -> (Bool, Html Msg)
cellHtml skip maybeSelection konceptRow cubeColumn attributes = 
            case maybeSelection of
               Just selection->
                  let
                     selectionFactor = createSelectionFactor selection
                     memberFactor = createMembersFactor cubeColumn
                  in 
                     case selection  of
                        Selecting _ -> selectionHtml skip selectionFactor konceptRow memberFactor attributes (CellCreator textCell)
                        Editing _-> selectionHtml skip selectionFactor konceptRow memberFactor attributes (CellCreator inputCell) 
               Nothing -> (skip, textCell "" attributes)

cell: Direction -> Area -> Maybe Selection -> ColumnIndex -> CubeColumn -> RowIndex -> KonceptRow -> (Bool,List (Html Msg)) -> (Bool,List (Html Msg))
cell direction area selection columnIndex column rowIndex row state  =
   let 
         selectionAttributes: List (Attribute Msg) -> (Bool, Html Msg) 
         selectionAttributes = cellHtml (first state) selection row column
         newCell =
            cellAreaAttributes direction area columnIndex rowIndex
            |> addAttr attrCell
            |> addAttr attrBox       
            |> attrEventEditCell (cellFactors row column ) row  
            -- |> attrEventSelectCell (cellFactors row column ) row         
            |> selectionAttributes

   
   in

      (first newCell, [ second newCell ] ++ (second state))


gridCells: Direction -> Maybe Selection -> CubeColumns -> CubeRows -> List (Html Msg)
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
         cells: List KonceptRow -> ColumnIndex -> CubeColumn -> (Bool,List (List (Html Msg))) -> (Bool,List (List (Html Msg)))
         cells rows columnIndex cubeColumn acc =
            let 
               selected = first acc
               newState: (Bool, List (Html Msg))
               newState = 
                   -- fold over rows (KonceptRow)
                   rows
                   |> Lists.foldi (\i state row-> cell direction area selection columnIndex cubeColumn (RowIndex (i + 1)) row state) (selected,[])
              
            in
               (first newState, [ second newState ] ++ (second acc))
              
      in
         --- fold over columns
         cubeColumns.columns
         |> Lists.foldi (\i state col -> (cells cubeRows.rows) (ColumnIndex (i + 1)) col state) (False,[])
         |> (\(_,result) -> result)
         |> List.concat


viewCube: Direction -> HyperCube -> List DimensionalKoncept -> Maybe Selection -> Html Msg
viewCube direction hyperCube koncepts selection =
    let 

         -- selectedFilter:(Maybe ValueKoncept, List Member)
         -- selectedFilter =
         --    case selection of
         --        Just (vk,m) -> (Just vk, m)
         --        Nothing -> (Nothing, [])
         selectionFactors = 
            case selection of
               Just s -> 
                  s
                  |> factorsFromSelection 
                  |> NList.toList
               Nothing -> []
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
            |> calculateCubeColumns direction selectionFactors
       
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
               |> List.map (\rowHeader -> rowHeaderCellIndented cubeColumns.offset selectionFactors rowHeader)

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




-- attrSelectCell: CubeColumn -> KonceptRow  -> List (Attribute Msg) -> List (Attribute Msg)
-- attrSelectCell (CubeColumn members) row attr = 
--    case row.item of
--       AbstractRow ak ->
--          attr 
--       ValueRow vk ->
--          let
--             event: Attribute Msg 
--             event = 
--                (vk,members |> NList.toList) 
--                |> SelectedCell 
--                |> Select 
--                |> onClick
--          in
--             [event] ++ attr

-- attrEditCell: CubeColumn -> KonceptRow  -> List (Attribute Msg) -> List (Attribute Msg)
-- attrEditCell (CubeColumn members) row attr = 
--    case row.item of
--       AbstractRow ak ->
--          attr 
--       ValueRow vk ->
--          let
--             event: Attribute Msg 
--             event = 
--                (vk,members |> NList.toList) 
--                |> SelectedCell 
--                |> Select 
--                |> onDoubleClick
--          in
--             [event] ++ attr

-- gridSizeAttribute:  String -> String -> Int -> String -> Attribute msg
-- gridSizeAttribute s1 s2 i s3 =
--    i 
--    |> String.fromInt
--    |> (\s -> s2 ++ s ++ s3)
--    |> style s1


-- type GridColumns = GridColumns Int
-- columnsInt: GridColumns -> Int
-- columnsInt (GridColumns columns) = columns

-- type GridRows = GridRows Int
-- rowsInt: GridRows -> Int
-- rowsInt (GridRows rows) = rows

-- grid: GridRows -> GridColumns -> List (Attribute msg) 
-- grid (GridRows rows) (GridColumns cols)=
--     let 
      
--         attrdisplay: Attribute msg
--         attrdisplay = style "display" "grid"  
--       -- Horizontal span
--         attrColumns: Attribute msg
--         attrColumns = gridSizeAttribute "grid-template-columns" "repeat(" cols ")"
--       -- vertical span
--         attrRows: Attribute msg
--         attrRows = gridSizeAttribute "grid-template-rows" "repeat(" rows ", minmax(50px,100px))"

--     in
--      [  attrdisplay , attrColumns ]



-- attributeGridArea: Area -> List (Attribute msg)
-- attributeGridArea  area =
--     let 
--         row: String 
--         row = area.verticalStart |> verticalStartToInt |> String.fromInt |> (\s -> s )
--         col: String 
--         col = area.horizontalStart |> horizontalStartToInt |> String.fromInt |> (\s -> " / " ++ s)
--         colSpan: String 
--         colSpan = area.horizontalSpan |> horizontalSpanToInt |> String.fromInt |> (\s ->" / span " ++ s )
--         rowSpan: String 
--         rowSpan = area.verticalSpan |> verticalSpanToInt |> String.fromInt |> (\s ->" / span " ++ s)
--         areaAttribute: Attribute msg   
--         areaAttribute = style "grid-area" (row ++ col ++ rowSpan ++ colSpan )
--     in
--          [ areaAttribute ]


-- attrBox: List (Attribute msg)
-- attrBox = [ style "border" "black 1px solid" ]


-- attrLeftIndent: Int -> List (Attribute msg)
-- attrLeftIndent indent =
--    if indent > 0 && indent < 11 then
--       [ class ("rind" ++ String.fromInt indent) ]
--    else
--       []

-- attrIndentHorizontalStart: HorizontalStart -> List (Attribute msg)
-- attrIndentHorizontalStart (HorizontalStart (Start start)) = attrLeftIndent start
   
-- addAttr: List (Attribute msg) -> List (Attribute msg) -> List (Attribute msg) 
-- addAttr a1 a2 =
--    List.append a1 a2

-- attrCell: List (Attribute msg)
-- attrCell  = 
--     [ class "grid-cell" ] 


-- attrSelected: String -> Bool -> List (Attribute msg)
-- attrSelected c b =
--    if b then [ class c ] else []

-- attrColumnHeaderSelected: Bool -> List (Attribute msg)
-- attrColumnHeaderSelected  = attrSelected "grid-cell-selected" 
   
-- attrRowHeaderSelected: Bool -> List (Attribute msg)
-- attrRowHeaderSelected  = attrSelected "grid-cell-selected"
  
-- attrCellPathToSelection: Bool -> List (Attribute msg)
-- attrCellPathToSelection = attrSelected "grid-cell-selected"
    
-- attrSelectedCell: Bool -> List (Attribute msg)
-- attrSelectedCell = attrSelected "grid-cell-selected-1"

-- attrMemberCell: Bool -> List (Attribute msg)
-- attrMemberCell = attrSelected "grid-cell-member"

-- textCell: String -> List (Attribute Msg) -> Html Msg
-- textCell s attr  =
--    div attr [ text s ]


-- columnHeaderCell: CubeColumnOffset -> CubeColumnHeader -> Html Msg
-- columnHeaderCell (CubeColumnOffset offset) colunmHeader =
   
--    colunmHeader.area
--    |> offsetArea offset
--    |> attributeGridArea
--    |> List.append attrCell
--    |> List.append attrBox
--    |> List.append (attrColumnHeaderSelected colunmHeader.isSelected)
--    |> textCell colunmHeader.member.name


-- attrRowHeaderAbstract: KonceptRow -> List (Attribute msg)
-- attrRowHeaderAbstract rowHeader = 
--    case tryGetValueKoncept rowHeader.item of
--       Nothing -> [ class "abstract-header" ]
--       Just _ -> []


-- rowHeaderCellIndented: CubeRowOffset -> Maybe ValueKoncept -> KonceptRow -> Html Msg
-- rowHeaderCellIndented (CubeRowOffset offset) selection rowHeader =
--    let    
        
--          newArea: Area
--          newArea =
--             rowHeader.area
--             |> Area.setHorizontalSpan (Span 1)  
--             |> Area.setHorizontalStart (Start 1)
--             |> offsetArea offset
--    in 
--       let 
--          selected =         
--             case selection of
--                Just vk ->
--                   case konceptRowFactor rowHeader.item of
--                      Just factor -> factor == vk.factor
--                      Nothing -> False
--                Nothing -> 
--                      False
--       in
--          attrCell
--          |> addAttr attrBox
--          |> addAttr (attrIndentHorizontalStart rowHeader.area.horizontalStart)
--          |> addAttr (attributeGridArea newArea)
--          |> addAttr (attrRowHeaderSelected selected)
--          |> addAttr (attrRowHeaderAbstract rowHeader)
--          |> textCell (konceptRowItemName rowHeader.item)   
      
-- factorFromMemberList: List Member -> Int
-- factorFromMemberList members =
--    case members of
--       [] -> 1
--       head :: tail ->
--           head.factor
--           |> factorToInt
--           |> (\i -> i * factorFromMemberList tail)

-- factorFromValue: ValueKoncept -> Int -> Int
-- factorFromValue vk mv =
--    vk.factor
--    |> factorToInt
--    |> (\i -> i * mv)

-- factorFromSelection: ValueKoncept -> List Member -> Int
-- factorFromSelection vk =
--    factorFromMemberList >> (factorFromValue vk)

-- -- TODO: Add
-- trySelectCell: Bool -> Maybe (ValueKoncept, List Member) -> KonceptRow -> CubeColumn -> List (Attribute Msg)
-- trySelectCell skip selection row (CubeColumn cellMembers) =
--    if skip then []
--    else
--       let
--          result =
--             case selection of
--                Nothing -> []
--                Just (vk,members) -> 
--                   case tryGetValueKoncept row.item of
--                      Just vkCell -> 
--                         let
--                            productSelection = factorFromSelection vk members
--                            productCell = factorFromSelection vkCell (cellMembers |> NList.toList)
--                         in

--                         attrSelectedCell (productSelection ==  productCell) ---(factor == vk.factor && members.head.factor == member.factor)
--                      Nothing -> []
--       in
--          result

-- trySelectMemberCell: Bool -> Maybe (ValueKoncept, List Member) -> KonceptRow -> CubeColumn -> List (Attribute Msg)
-- trySelectMemberCell skip selection row (CubeColumn cellMembers) =
--    if skip then []
--    else
--       let 
--          result =
--             case selection of
--                Nothing -> []
--                Just (vk,members) -> 
--                   let
--                      productSelection = factorFromMemberList members
--                      productCell = factorFromMemberList (cellMembers |> NList.toList)
--                   in
--                      case tryGetValueKoncept row.item of
--                         Just vkCell -> 
--                            attrMemberCell ((productSelection ==  productCell) || vkCell.factor == vk.factor)  ---(factor == vk.factor && members.head.factor == member.factor)
--                         Nothing -> 
--                            attrMemberCell (productSelection ==  productCell)
--       in 
--          result

-- calculateIfCellIsSelected: Maybe (ValueKoncept, List Member) -> KonceptRow -> CubeColumn -> Bool
-- calculateIfCellIsSelected selection konceptRow (CubeColumn cellMembers) =
--    case selection of
--       Nothing -> False
--       Just (vk,members) -> 
--           case tryGetValueKoncept konceptRow.item of
--             Just vkCell -> 
--                let
--                    productSelection = factorFromSelection vk members
--                    productCell = factorFromSelection vkCell (cellMembers |> NList.toList)
--                in
--                productSelection == productCell ---(factor == vk.factor && members.head.factor == member.factor)
--             Nothing -> False

-- cell: Direction -> Area -> Maybe (ValueKoncept, List Member) -> Int -> CubeColumn -> Int -> KonceptRow -> (Bool,List (Html Msg)) -> (Bool,List (Html Msg))
-- cell direction area selection columnIndex column rowIndex row state  =
--    let 
       
--       skipSelection = first state
--       newSelectionState = 
--          if (first state) then True
--          else calculateIfCellIsSelected selection row column

--       newCell: Html Msg
--       newCell =
--          case direction of
--             Horizontal -> 
--                area
--                |> Area.addHorizontalStart (columnIndex |> Start |> HorizontalStart)
--                |> Area.addVerticalStart (rowIndex |> Start |> VerticalStart)
--                |> attributeGridArea 
--                |> addAttr attrCell
--                |> addAttr attrBox
--                |> addAttr (trySelectCell skipSelection selection row column)  
--                |> addAttr (trySelectMemberCell skipSelection selection row column)  
--                |> attrSelectCell column row                      
--                |> textCell ""

--             Vertical ->
--                area
--                |> Area.addVerticalStart (columnIndex |> Start |> VerticalStart)
--                |> Area.addHorizontalStart (rowIndex |> Start |> HorizontalStart)
--                |> attributeGridArea 
--                |> addAttr attrCell
--                |> addAttr attrBox
--                |> addAttr (trySelectCell skipSelection selection row column)  
--                |> addAttr (trySelectMemberCell skipSelection selection row column)  
--                |> attrSelectCell column row
--                |> textCell ""
--    in

--       (newSelectionState, [ newCell ] ++ (second state))


-- gridCells: Direction -> Maybe (ValueKoncept, List Member) -> CubeColumns -> CubeRows -> List (Html Msg)
-- gridCells direction selection cubeColumns cubeRows =
--    let 
--       area: Area 
--       area = 
--          Area.emptyArea 
--          -- add offset for columns and rows
--          |> offsetArea (cubeRowOffsetToOffset cubeColumns.offset) 
--          |> offsetArea (cubeColumnOffsetToOffset cubeRows.offset)
--          |> Area.addVerticalSpan Area.oneVerticalSpan
--          |> Area.addHorizontalSpan Area.oneHorizontalSpan
--    in  
--       let    
--          cells: List KonceptRow -> Int -> CubeColumn -> (Bool,List (List (Html Msg))) -> (Bool,List (List (Html Msg)))
--          cells rows columnIndex cubeColumn acc =
--             let 
--                selected = first acc
--                newState: (Bool, List (Html Msg))
--                newState = 
--                    -- fold over rows (KonceptRow)
--                    rows
--                    |> Lists.foldi (\i state row-> cell direction area selection columnIndex cubeColumn (i + 1) row state) (selected,[])
              
--             in
--                (first newState, [ second newState ] ++ (second acc))
              
--       in
--          --- fold over columns
--          cubeColumns.columns
--          |> Lists.foldi (\i state col -> (cells cubeRows.rows) (i + 1) col state) (False,[])
--          |> (\(_,result) -> result)
--          |> List.concat


-- viewCube: Direction -> HyperCube -> List DimensionalKoncept -> Maybe (ValueKoncept, List Member) -> Html Msg
-- viewCube direction hyperCube koncepts selection =
--     let 

--          selectedFilter:(Maybe ValueKoncept, List Member)
--          selectedFilter =
--             case selection of
--                 Just (vk,m) -> (Just vk, m)
--                 Nothing -> (Nothing, [])

--          dimensions: List Dimension 
--          dimensions = 
--             hyperCube.dimensions 
--             |> NList.map hyperDimensionAsDimension
--             |> NList.toList

--          span: Span 
--          span = dimensions |> calculateSpanForDimensions 

--          cubeRows: CubeRows
--          cubeRows = calculateIndentedCubeRows koncepts  

--          cubeColumns: CubeColumns
--          cubeColumns = 
--             dimensions
--             |> calculateCubeColumns direction (second selectedFilter)
       
--          columns: List (Html Msg) 
--          columns = 
--             cubeColumns.headers 
--             |> List.map (\header -> columnHeaderCell cubeRows.offset header)

--          cells: List (Html Msg) 
--          cells = 
--             gridCells direction selection cubeColumns cubeRows

--          rowHeaders: List (Html Msg)
--          rowHeaders =
--                cubeRows.rows
--                |> List.map (\rowHeader -> rowHeaderCellIndented cubeColumns.offset (first selectedFilter) rowHeader)

--          gridRows: Direction -> Span -> List CubeColumn -> GridRows
--          gridRows d (Span s) cols  =
--             case d of
--                Horizontal ->
--                   GridRows s
--                Vertical ->
--                   GridRows (List.length cols)
        
--          gridColumns: Direction -> Span -> List CubeColumn -> GridColumns
--          gridColumns d (Span s) cols =
--             case d of
--                Horizontal ->
--                   GridColumns (List.length cols)
--                Vertical ->
--                   GridColumns s

         
               
--     in
--         div (grid (gridRows direction span cubeColumns.columns) (gridColumns direction span cubeColumns.columns)) (columns ++ rowHeaders ++ cells)