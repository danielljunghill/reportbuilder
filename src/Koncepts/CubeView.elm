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

attrEventSelectCell: CubeColumn -> ValueKoncept -> List (Attribute Msg) -> List (Attribute Msg)
attrEventSelectCell cubeColumn row attr = 
         let
            event: Attribute Msg 
            event = 
               rowAndMemberFactor row cubeColumn
               |> SelectValue
               |> Msg.SelectMsg  
               |> onClick
         in
            [event] ++ attr

attrEventEditCell: Content -> CubeColumn ->  ValueKoncept  -> List (Attribute Msg) -> List (Attribute Msg)
attrEventEditCell content cubeColumn row attr = 
         let
            event: Attribute Msg 
            event = 
               rowAndMemberFactor row cubeColumn
               |> EditValue
               |> Msg.SelectMsg 
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

attrSelected2: List (Attribute Msg)-> Bool -> List (Attribute Msg)
attrSelected2 attr selected =
   if selected then attr else []
columnHeaderCell: CubeColumnOffset -> CubeColumnHeader -> Html Msg
columnHeaderCell (CubeColumnOffset offset) colunmHeader =
   colunmHeader.area
   |> offsetArea offset
   |> attributeGridArea
   |> List.append attrCell
   |> List.append attrBox
   |> List.append (attrSelected2 attColumnHeaderSelected colunmHeader.isSelected)
   |> textCell (Content colunmHeader.member.name)

attrRowHeaderAbstract: KonceptRow -> List (Attribute msg)
attrRowHeaderAbstract rowHeader = 
   case tryGetValueKoncept rowHeader.item of
      Nothing -> [ class "abstract-header" ]
      Just _ -> []

attrRowHeaderSelected  =  "grid-row-header-selected" |> createClassAttr

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
         |> addAttr (attrSelected2 attrRowHeaderSelected selected)
         |> addAttr (attrRowHeaderAbstract rowHeader)
         |> textCell (Content (konceptRowItemName rowHeader.item))


membersToFactors members =
   members |> NList.map (\m -> m.factor)
membersToSingleFactor =
   membersToFactors >> multiplyAllFactors

multiplyFactor (Factor a) (Factor b) = Factor (a * b)

cubeColumnSingeFactor (CubeColumn members) =
   members
   |> membersToSingleFactor

rowAndMemberFactor: ValueKoncept -> CubeColumn -> NList Factor
rowAndMemberFactor vk (CubeColumn members) =
    members 
    |> membersToFactors
    |> NList.addFirst vk.factor 

multiplyAllFactors: NList Factor -> Factor
multiplyAllFactors factors = factors |> NList.fold multiplyFactor (Factor 1) 

selectionFactors: Selection -> NList Factor
selectionFactors selection =
   case selection of
      EditValue edited -> edited
      SelectValue selected -> selected

type SelectionFactor = SelectionFactor Factor

selectionSingleFactor:  Selection ->  SelectionFactor
selectionSingleFactor  =
   selectionFactors 
   >> multiplyAllFactors 
   >> SelectionFactor


type alias SelectionWithFactor =
   {
         selection: Selection
      ,  factor: SelectionFactor
   }

createSelectionWithFactor: Selection -> SelectionWithFactor
createSelectionWithFactor selection =
   {
         selection = selection
      ,  factor = selectionSingleFactor selection
   }

isFactorSelection: SelectionFactor -> Factor -> Bool
isFactorSelection (SelectionFactor sf) f =
   sf == f


modFactors (SelectionFactor (Factor taljare)) (Factor namnare)  = modBy namnare taljare

type alias CellHtml =
   {
         isSelected: Bool
      ,  html: CellCreator
   }

type CellCreator = CellCreator (List (Attribute Msg) -> Html Msg)

createClassAttr name = [ class name ]

attColumnHeaderSelected = "grid-column-header-selected" |> createClassAttr
attrSelectedCell =  "grid-cell-selected" |> createClassAttr
attrAssociatedCell = "grid-cell-associated" |> createClassAttr

associatedCell:  ValueKoncept -> CubeColumn -> Content-> CellCreator
associatedCell valueKoncept cubeColumn content =
   (\attr -> 
      attrAssociatedCell 
      |> addAttr attr
      |> (attrEventEditCell content cubeColumn valueKoncept)
      |> (attrEventSelectCell cubeColumn valueKoncept)
      |> textCell content)
   |> CellCreator

selectedCell: Selection -> ValueKoncept -> CubeColumn->  Content-> CellCreator
selectedCell selection valueKoncept cubeColumn content  =
   case selection of
      EditValue _ ->
               inputCell content 
               |> CellCreator
      SelectValue _ ->
               (\attr -> 
                  attrEventEditCell content cubeColumn valueKoncept attr
                  |> textCell content)
               |> CellCreator
             

-- editCell: ValueKoncept -> CubeColumn -> Content -> CellCreator
-- editCell valueKoncept cubeColumn content  =
--    inputCell content
--    |> CellCreator

normalCell : ValueKoncept -> CubeColumn -> Content -> CellCreator
normalCell  valueKoncept cubeColumn content =
   (\attr -> 
      attrEventEditCell content cubeColumn valueKoncept attr
      |> (attrEventSelectCell cubeColumn valueKoncept)
      |> textCell content)
   |> CellCreator

normalAbstractCell : CellCreator
normalAbstractCell =
   textCell (Content "")
   |> CellCreator

associatedAbstractCell : CellCreator
associatedAbstractCell =
   (\attr ->
      attr
      |> addAttr attrAssociatedCell
      |> textCell (Content "")
   )
   |> CellCreator

textCell: Content -> List (Attribute Msg) -> Html Msg
textCell (Content content) attr  =
   div attr [ text content ]

inputCell: Content -> List (Attribute Msg) -> Html Msg
inputCell (Content content) attr  =
    input ([ value content ] ++ attr) []


cellHtml: Bool -> CellCreator -> CellHtml
cellHtml selected cellCreator =
      {
            isSelected = selected
         ,  html = cellCreator
      }

  
gridCellWithSelection: Bool -> SelectionWithFactor -> KonceptRow -> CubeColumn -> CellHtml
gridCellWithSelection skip selectionFactor konceptRow cubeColumns =
      case konceptRow.item of
         AbstractRow _ ->
            if skip then
               cellHtml skip normalAbstractCell
            else 
               if (modFactors selectionFactor.factor (cubeColumnSingeFactor cubeColumns) == 0) then 
                  cellHtml skip associatedAbstractCell
               else
                  cellHtml skip normalAbstractCell
         ValueRow row ->
               if skip then
                  Content ""
                  |> normalCell row cubeColumns
                  |> cellHtml skip 
               else 
                  let
                     cubeFactor = cubeColumnSingeFactor cubeColumns
                     totalFactor = multiplyFactor cubeFactor row.factor
                  in
                     if isFactorSelection selectionFactor.factor totalFactor then
                        Content ""
                        |> selectedCell selectionFactor.selection row cubeColumns
                        |> cellHtml True
                     else
                        if (modFactors selectionFactor.factor row.factor == 0) || (modFactors selectionFactor.factor cubeFactor == 0) then
                           Content ""
                           |> associatedCell row cubeColumns
                           |> cellHtml skip 
                        else
                           Content ""
                           |> normalCell row cubeColumns
                           |> cellHtml skip 

gridCellWithoutSelection: KonceptRow -> CubeColumn -> CellHtml 
gridCellWithoutSelection konceptRow cubeColumns =
         case konceptRow.item of
            AbstractRow _ ->
               normalAbstractCell
               |> cellHtml False 
            ValueRow row ->
               Content ""
               |> normalCell row cubeColumns
               |> cellHtml False 

type RowIndex = RowIndex Int
type ColumnIndex = ColumnIndex Int

createGridCell: Bool -> Maybe SelectionWithFactor -> KonceptRow -> CubeColumn -> CellHtml
createGridCell skip maybeSelectionWithFactor konceptRow cubeColumns =
   case maybeSelectionWithFactor of
      Just selection ->  gridCellWithSelection skip selection  konceptRow cubeColumns
      Nothing -> gridCellWithoutSelection konceptRow cubeColumns

creatorCreateCell: CellCreator -> List (Attribute Msg) -> Html Msg 
creatorCreateCell (CellCreator creator) attr =
   creator attr

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


cell: Direction -> Area -> Maybe SelectionWithFactor -> ColumnIndex -> CubeColumn -> RowIndex -> KonceptRow -> (Bool,List (Html Msg)) -> (Bool,List (Html Msg))
cell direction area maybeSelection columnIndex column rowIndex row state  =

   let
      attr =
         cellAreaAttributes direction area columnIndex rowIndex
         |> addAttr attrCell
         |> addAttr attrBox

      htmlCell =  createGridCell (first state) maybeSelection row column  
        
   in  

      (htmlCell.isSelected, [ creatorCreateCell htmlCell.html attr ] ++ (second state))


gridCells: Direction -> Maybe Selection -> CubeColumns -> CubeRows -> List (Html Msg)
gridCells direction selection cubeColumns cubeRows =
   let 
      selectionWithFactors =
         selection
         |> Maybe.map createSelectionWithFactor
                
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
                   |> Lists.foldi (\i state row-> cell direction area selectionWithFactors columnIndex cubeColumn (RowIndex (i + 1)) row state) (selected,[])
              
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
         factorsForSelection: List Factor
         factorsForSelection = 
            case selection of
               Just s -> 
                 selectionFactors s
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
            |> calculateCubeColumns direction factorsForSelection
       
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
               |> List.map (\rowHeader -> rowHeaderCellIndented cubeColumns.offset factorsForSelection rowHeader)

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