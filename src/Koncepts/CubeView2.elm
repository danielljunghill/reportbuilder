module Koncepts.CubeView2 exposing (..)
-- import Koncepts.CubeKoncept exposing (..)
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
import Koncepts.CubeModel exposing (..)
import Koncepts.CubeRow exposing (..)
import Koncepts.CubeRows exposing(..)
import Koncepts.CubeRowHeader exposing(..)
import Koncepts.Factorhandling exposing(..)

type Orientation =
   CubeRowsAsRows
   | CubeRowsAsColumns

factorsForCell: CubeColumn -> CubeValueRow ->  NList Factor
factorsForCell  (CubeColumn members) (CubeValueRow (valueKoncept,rowContext)) =
        let
            factors =
               valueKoncept.factor
               |> NList.create
               |> NList.addList  (membersFactorList ((rowContext.members) ++ (members |> NList.toList)))
               
        in
             factors

msgFactorsForCell: CubeColumn -> CubeValueRow ->  (NList Factor, List AbstractFactor)
msgFactorsForCell  cubeColumn cubeValueRow =
             (factorsForCell cubeColumn cubeValueRow, cubeValueRowAbstractFactors cubeValueRow)

attrEventSelectCell: CubeColumn -> CubeValueRow -> List (Attribute Msg) -> List (Attribute Msg)
attrEventSelectCell cubeColumn cubeValueRow attr = 
        let

            event:  Attribute Msg
            event = 
               cubeValueRow
               |> msgFactorsForCell cubeColumn 
               |> SelectValue 
               |> Msg.SelectMsg 
               |> onClick
             
        in
            [ event ] ++ attr

attrEventEditCell: CubeColumn ->  CubeValueRow  -> List (Attribute Msg) -> List (Attribute Msg)
attrEventEditCell cubeColumn cubeValueRow attr = 
         let
            event:Attribute Msg
            event = 
               cubeValueRow
               |> msgFactorsForCell cubeColumn 
               |> EditValue 
               |> Msg.SelectMsg 
               |> onDoubleClick
               
         in
            [ event ] ++ attr

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

grid: List (Attribute msg) 
grid =
    let 
      
        attrdisplay: Attribute msg
        attrdisplay = style "display" "grid"  


    in
     [  attrdisplay ]

attributeGridArea: Area -> List (Attribute msg)
attributeGridArea area =
    let 
        row: String 
        row = area.row |> intRow |> String.fromInt |> (\s -> s )
        col: String 
        col = area.column |>  intColumn |> String.fromInt |> (\s -> " / " ++ s)
        colSpan: String 
        colSpan = area.columnSpan |> intColumnSpan |> String.fromInt |> (\s ->" / span " ++ s )
        rowSpan: String 
        rowSpan = area.rowSpan |> intRowSpan |> String.frfomInt |> (\s ->" / span " ++ s)
        areaAttribute: Attribute msg   
        areaAttribute = style "grid-area" (row ++ col ++ rowSpan ++ colSpan )
    in
         [ areaAttribute ]


attrBox: List (Attribute msg)
attrBox = [ style "border" "black 1px solid" ]




-- attrIndentHorizontalStart: HorizontalStart -> List (Attribute msg)
-- attrIndentHorizontalStart (HorizontalStart (Start start)) = attrLeftIndent start
   
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

attrColumnHeaderSelected = "grid-column-header-selected" |> createClassAttr
attrColumnHeader = "grid-column-header" |> createClassAttr

adjustAreaToOrientation: Orientation -> Area -> Area
adjustAreaToOrientation orientation area =
   case orientation of
      CubeRowsAsRows -> area
      CubeRowsAsColumns ->
         {
               row = area.column |> intColumn |> Row
            ,  column = area.row |> intRow |> Column
            ,  rowSpan = area.columnSpan |> intColumnSpan |> RowSpan
            ,  columnSpan = area.rowSpan |> intRowSpan |> ColumnSpan
         }

offsetAndAdjustToOrientation: Orientation -> Offset -> Area -> Area 
offsetAndAdjustToOrientation orientation offset area =
   case orientation of
      CubeRowsAsRows -> 
         area
         |> offsetArea offset
      CubeRowsAsColumns ->
         let 
            newOffset =
               {
                     column = offset.row |> intRow |> Column
                  ,  row = offset.column |> intColumn |> Row
               }
         in
            area
            |> offsetArea newOffset
         

columnHeaderCell: Orientation -> CubeColumnOffset -> CubeHeader -> Html Msg
columnHeaderCell orientation (CubeColumnOffset offset) cubeHeader =
   cubeHeader.area
   |> adjustAreaToOrientation orientation
   |> offsetAndAdjustToOrientation orientation offset
   |> attributeGridArea
   |> List.append attrColumnHeader
   |> List.append attrBox
   |> List.append (attrSelected2 attrColumnHeaderSelected cubeHeader.isSelected)
   |> textCell (Content cubeHeader.name)

attrRowHeaderAbstract: CubeRow -> List (Attribute msg)
attrRowHeaderAbstract row = 
   case row of
      AbstractRow _ -> [ class "abstract-header" ]
      _ -> []

attrRowHeaderSelected  =  "grid-row-header-selected" |> createClassAttr
attrRowHeader = "grid-row-header" |> createClassAttr


cubeHeaderAttributes: CubeHeader -> List (Attribute msg)
cubeHeaderAttributes cubeHeader =
   cubeHeader.attributes
   |> List.map class

attrLeftIndent: Maybe Indent -> List (Attribute msg)
attrLeftIndent maybeIndent =
   case maybeIndent of
      Just (Indent indent) ->
         if indent > 0 && indent < 11 then
            [ class ("rind" ++ String.fromInt indent) ]
         else
            []
      Nothing -> []

rowHeaderCellIndented: CubeRowOffset -> List Factor -> CubeRowHeader -> Html Msg
rowHeaderCellIndented (CubeRowOffset offset) selection (CubeRowHeader cubeHeader) =
   let    
        
         newArea: Area
         newArea =
            cubeHeader.area
            |> adjustAreaToOrientation CubeRowsAsRows
            |> offsetAndAdjustToOrientation CubeRowsAsRows offset
            |> addRowToArea (Row 1)

   in 
   
         attrCell
         |> addAttr attrBox
         |> addAttr (attrLeftIndent cubeHeader.indent)
         |> addAttr (attributeGridArea newArea)
         |> addAttr attrRowHeader
         |> addAttr (attrSelected2 attrRowHeaderSelected cubeHeader.isSelected )
         |> List.append (cubeHeaderAttributes cubeHeader)
         |> textCell (Content (cubeHeader.name))


cubeColumnSingeFactor (CubeColumn members) =
   members
   |> NList.toList
   |> membersAsFactor

rowAndMemberFactorList: CubeValueRow -> CubeColumn -> NList Factor
rowAndMemberFactorList (CubeValueRow (valueKoncept,rowContext)) (CubeColumn members) =
      let
         totalMembers = rowContext.members ++  (members |> NList.toList) 
      in 
         valueKoncept.factor
         |> NList.create 
         |> NList.addList (membersFactorList totalMembers) 
      



type SelectionFactor = SelectionFactor Factor

selectionSingleFactor:  Selection ->  SelectionFactor
selectionSingleFactor  =
   factorsInSelection 
   >> multiplyFactors 
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


attrSelectedCell =  "grid-cell-selected" |> createClassAttr
attrAssociatedCell = "grid-cell-associated" |> createClassAttr

associatedCell:  CubeValueRow -> CubeColumn -> Content-> CellCreator
associatedCell cubeValueRow cubeColumn content =
   (\attr -> 
      attrAssociatedCell 
      |> addAttr attr
      |> (attrEventEditCell cubeColumn cubeValueRow)
      |> (attrEventSelectCell cubeColumn cubeValueRow)
      |> textCell content)
   |> CellCreator

selectedCell: Selection -> CubeValueRow -> CubeColumn->  Content-> CellCreator
selectedCell selection cubeValueRow cubeColumn content  =
   case selection of
      EditValue _ ->
               cubeColumn
               |> rowAndMemberFactorList cubeValueRow
               |> inputCell content 
               |> CellCreator
      SelectValue _ ->
               (\attr -> 
                  attrEventEditCell cubeColumn cubeValueRow attr
                  |> addAttr attrSelectedCell
                  |> textCell content)
               |> CellCreator
             

normalCell : CubeValueRow -> CubeColumn -> Content -> CellCreator
normalCell  cubeValueRow cubeColumn content =
   (\attr -> 
      attrEventEditCell cubeColumn cubeValueRow attr
      |> (attrEventSelectCell cubeColumn cubeValueRow)
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

getContentValueFromFetcher: ValueFetcher -> List Factor -> Content
getContentValueFromFetcher (ValueFetcher vf) factors = 
      factors
         |> multiplyFactors
         |> ReportedValueFactor
         |> vf
         |> Maybe.map reportValueContent
         |> Maybe.withDefault (Content "")

textCell: Content -> List (Attribute Msg) -> Html Msg
textCell (Content content) attr  =
   div attr [ text content ]

inputCell: Content -> NList Factor -> List (Attribute Msg) -> Html Msg
inputCell (Content content) factors attr =
    let

       msg s = 
         (factors,Content s)
         |> UpdateValue
         |> UpdateValueMsg 
    in
      input ([ value content , onInput msg ] ++ attr) []


cellHtml: Bool -> CellCreator -> CellHtml
cellHtml selected cellCreator =
      {
            isSelected = selected
         ,  html = cellCreator
      }


getContentFromRowAndMembers: CubeValueRow -> CubeColumn -> ValueFetcher -> Content 
getContentFromRowAndMembers cubeValueRow cubeColumn valueFetcher =
         cubeValueRow
         |> factorsForCell cubeColumn
         |> NList.toList
         |> getContentValueFromFetcher valueFetcher

gridCellWithSelection: ValueFetcher -> Bool -> SelectionWithFactor -> CubeRow -> CubeColumn -> CellHtml
gridCellWithSelection valueFetcher skip selectionFactor cubeRow cubeColumn =
      -- calculate factor for cell
      case cubeRow of
         AbstractRow cubeAbstractRow ->
            if skip then
               cellHtml skip normalAbstractCell
            else 
               -- TODO calculate factor foor either row or column
               if (modFactors selectionFactor.factor (cubeColumnAsFactor cubeColumn) == 0) then 
                  cellHtml skip associatedAbstractCell
               else
                  cellHtml skip normalAbstractCell
         ValueRow cubeValueRow ->
               let
                  content = 
                     valueFetcher
                     |> getContentFromRowAndMembers cubeValueRow cubeColumn
               in
                  if skip then
                     content
                     |> normalCell cubeValueRow cubeColumn
                     |> cellHtml skip 
                  else 
                     let
                        cubeColumnFactor = cubeColumnAsFactor cubeColumn
                        totalFactor = 
                           cubeValueRow 
                           |> factorsForCell cubeColumn 
                           |> NList.toList
                           |> multiplyFactors 
                     in
                        if isFactorSelection selectionFactor.factor totalFactor then
                          content
                           |> selectedCell selectionFactor.selection cubeValueRow cubeColumn
                           |> cellHtml True
                        else
                           if (modFactors selectionFactor.factor (cubeValueRow |> cubeValueRowFactors |> NList.toList |> multiplyFactors) == 0) || (modFactors selectionFactor.factor cubeColumnFactor == 0) then
                              content
                              |> associatedCell cubeValueRow cubeColumn
                              |> cellHtml skip 
                           else
                              content
                              |> normalCell cubeValueRow cubeColumn
                              |> cellHtml skip 

gridCellWithoutSelection: ValueFetcher -> CubeRow -> CubeColumn -> CellHtml 
gridCellWithoutSelection valueFetcher cubeRow cubeColumn =
      case cubeRow of
         AbstractRow _ ->
            normalAbstractCell  
            |> cellHtml False 
         ValueRow cubeValueRow ->
            valueFetcher
            |> getContentFromRowAndMembers cubeValueRow cubeColumn
            -- TODO split cubeRow into value and CubeRowMembers
            |> normalCell cubeValueRow cubeColumn
            |> cellHtml False 

type RowIndex = RowIndex Int
type ColumnIndex = ColumnIndex Int

createGridCell: ValueFetcher -> Bool -> Maybe SelectionWithFactor -> CubeRow -> CubeColumn -> CellHtml
createGridCell valueFecher skip maybeSelectionWithFactor cubeRow cubeColumn =
   case maybeSelectionWithFactor of
      Just selection ->  gridCellWithSelection valueFecher skip selection  cubeRow cubeColumn
      Nothing -> gridCellWithoutSelection valueFecher cubeRow cubeColumn

creatorCreateCell: CellCreator -> List (Attribute Msg) -> Html Msg 
creatorCreateCell (CellCreator creator) attr =
   creator attr

cellAreaAttributes: Orientation -> Area -> ColumnIndex -> RowIndex -> List (Attribute Msg)
cellAreaAttributes orientation area (ColumnIndex colIndex) (RowIndex rowIndex) =
   case orientation of
      CubeRowsAsRows  ->
         area
         |> Area.addColumnToArea (colIndex |> Column)
         |> Area.addRowToArea (rowIndex |> Row)
         |> attributeGridArea 
      CubeRowsAsColumns ->
         area
         |> Area.addRowToArea (colIndex |> Row)
         |> Area.addColumnToArea (rowIndex |> Column)
         |> attributeGridArea 


cell: ValueFetcher -> Orientation -> Area -> Maybe SelectionWithFactor -> ColumnIndex -> CubeColumn -> RowIndex -> CubeRow -> (Bool,List (Html Msg)) -> (Bool,List (Html Msg))
cell valueFetcher orientation area maybeSelection columnIndex cubeColumn rowIndex cubeRow state  =
   let
      attr =
         cellAreaAttributes orientation area columnIndex rowIndex
         |> addAttr attrCell
         |> addAttr attrBox
         |> Debug.log "attribute for area"

      htmlCell =  createGridCell valueFetcher (first state) maybeSelection cubeRow cubeColumn        
   in  
      (htmlCell.isSelected, [ creatorCreateCell htmlCell.html attr ] ++ (second state))


gridCells: ValueFetcher -> Orientation -> Maybe Selection -> CubeColumns -> CubeRows -> List (Html Msg)
gridCells valueFetcher orientation selection cubeColumns cubeRows =
   let 
      selectionWithFactors =
         selection
         |> Maybe.map createSelectionWithFactor

      --- BASE AREA FOR CELL row and column incremented in fold          
      area: Area 
      area = 
         Area.zeroArea 
         -- add offset for columns and rows
         |> offsetArea (cubeRowOffsetToOffset cubeColumns.offset) 
         |> offsetArea (cubeColumnOffsetToOffset cubeRows.offset)
         |> addRowSpanToArea (RowSpan 1)
         |> addColumnSpanToArea (ColumnSpan 1)

   in  
      let    
         cells: List CubeRow -> ColumnIndex -> CubeColumn -> (Bool,List (List (Html Msg))) -> (Bool,List (List (Html Msg)))
         cells rows columnIndex cubeColumn acc =
            let 
               selected = first acc
               newState: (Bool, List (Html Msg))
               newState = 
                   -- fold over rows (KonceptRow)
                   rows
                   |> Lists.foldi (\i state cubeRow -> cell valueFetcher orientation area selectionWithFactors columnIndex cubeColumn (RowIndex (i + 1)) cubeRow state) (selected,[])
              
            in
               (first newState, [ second newState ] ++ (second acc))
              
      in
         --- fold over columns
         cubeColumns.columns
         |> Lists.foldi (\i state col -> (cells cubeRows.rows) (ColumnIndex (i + 1)) col state) (False,[])
         |> (\(_,result) -> result)
         |> List.concat


viewCube: ValueFetcher -> Orientation -> HyperCube -> List DimensionalKoncept -> Maybe Selection -> Html Msg
viewCube valueFetcher orientation hyperCube koncepts selection =
    let 

         factorsForSelection: List Factor
         factorsForSelection = 
            case selection of
               Just s -> 
                 factorsInSelection s
               Nothing -> []

         dimensions: List Dimension 
         dimensions = 
            hyperCube.dimensions 
            |> NList.map hyperDimensionAsDimension
            |> NList.toList

         -- span: Span 
         -- span = dimensions |> calculateSpanForDimensions 

         cubeRows: CubeRows
         cubeRows = createCubeRowsIndented selection koncepts  

         cubeColumns: CubeColumns
         cubeColumns = 
            dimensions
            |> calculateCubeColumns selection  
       
         columns: List (Html Msg) 
         columns = 
            cubeColumns.headers 
            |> List.map (\header -> columnHeaderCell orientation cubeRows.offset header)

         cells: List (Html Msg) 
         cells = 
            gridCells valueFetcher orientation selection cubeColumns cubeRows

         rowHeaders: List (Html Msg)
         rowHeaders =
               cubeRows.headers
               |> List.map (\rowHeader -> rowHeaderCellIndented cubeColumns.offset factorsForSelection rowHeader)
             
    in
        div grid (columns ++ rowHeaders ++ cells)

