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
import Koncepts.Factorhandling exposing(..)


factorsForCell: CubeColumn -> CubeValueRow ->  NList Factor
factorsForCell  (CubeColumn members) (CubeValueRow (valueKoncept,rowContext)) =
        let
            factors =
                rowContext.members 
                |> List.map (\m -> m.factor)
                |> NList.addList (NList.create valueKoncept.factor)
                |> NList.append (membersFactorList members)
        in
             factors

msgFactorsForCell: CubeColumn -> CubeValueRow ->  (NList Factor, List AbstractFactor)
msgFactorsForCell  cubeColumn cubeValueRow =
             (factorsForCell cubeColumn cubeValueRow, factorsForCell cubeValueRow)

attrEventSelectCell: CubeColumn -> CubeValueRow -> List (Attribute Msg) -> List (Attribute Msg)
attrEventSelectCell cubeColumn cubeValueRow attr = 
        let

            event: List (Attribute Msg) 
            event = 
               msgFactorsForCell cubeColumn cubeValueRow
               |> SelectValue |> Msg.SelectMsg |> onClick
             
        in
            [ event ] ++ attr

attrEventEditCell: CubeColumn ->  CubeValueRow  -> List (Attribute Msg) -> List (Attribute Msg)
attrEventEditCell cubeColumn cubeRow attr = 
         let
            event:List (Attribute Msg)
            event = 
               msgFactorsForCell cubeColumn cubeRow
               |> EditValue |> Msg.SelectMsg |> onDoubleClick
               
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


attrLeftIndent: Maybe Int -> List (Attribute msg)
attrLeftIndent maybeIndent =
   case maybeIndent of
      Just indent ->
         if indent > 0 && indent < 11 then
            [ class ("rind" ++ String.fromInt indent) ]
         else
            []
      Nothing -> []

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

attrColumnHeaderSelected = "grid-column-header-selected" |> createClassAttr
attrColumnHeader = "grid-column-header" |> createClassAttr

columnHeaderCell: Direction -> CubeColumnOffset -> CubeHeader -> Html Msg
columnHeaderCell direction (CubeColumnOffset offset) cubeHeader =
   cubeHeader
   |> cubeHeaderToArea direction
   |> offsetArea offset
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

rowHeaderCellIndented: CubeRowOffset -> List Factor -> CubeRowHeader -> Html Msg
rowHeaderCellIndented (CubeRowOffset offset) selection (CubeRowHeader cubeHeader) =
   let    
        
         newArea: Area
         newArea =
            cubeHeaderToArea cubeHeader
            |> Area.offsetArea offset
   in 
   
         attrCell
         |> addAttr attrBox
         |> addAttr (attrIndentHorizontalStart cubeHeader.indent)
         |> addAttr (attributeGridArea newArea)
         |> addAttr attrRowHeader
         |> addAttr (attrSelected2 attrRowHeaderSelected cubeHeader.isSelected )
         |> List.append (cubeHeaderAttributes cubeHeader)
         |> textCell (Content (cubeHeader.name))


cubeColumnSingeFactor (CubeColumn members) =
   members
   |> membersFactor

rowAndMemberFactorList: CubeValueRow -> CubeColumn -> NList Factor
rowAndMemberFactorList (CubeValueRow (valueKoncept,rowContext)) (CubeColumn members) =
      rowContext.members 
      |> List.append (members |> NList.toList )  
      |> hyperValueFactorList valueKoncept  


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

associatedCell:  CubeRow -> CubeColumn -> Content-> CellCreator
associatedCell cubeRow cubeColumn content =
   (\attr -> 
      attrAssociatedCell 
      |> addAttr attr
      |> (attrEventEditCell cubeColumn cubeRow)
      |> (attrEventSelectCell cubeColumn cubeRow)
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
             

normalCell : CubeRow -> CubeColumn -> Content -> CellCreator
normalCell  cubeRow cubeColumn content =
   (\attr -> 
      attrEventEditCell cubeColumn cubeRow attr
      |> (attrEventSelectCell cubeColumn cubeRow)
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

getContentValueFromFetcher: ValueFetcher -> NList Factor -> Content
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
         |> getContentValueFromFetcher valueFetcher

gridCellWithSelection: ValueFetcher -> Bool -> SelectionWithFactor -> CubeRow -> CubeColumn -> CellHtml
gridCellWithSelection valueFetcher skip selectionFactor cubeRow cubeColumns =
      -- calculate factor for cell
      case cubeRow of
         AbstractRow cubeAbstractRow ->
            if skip then
               cellHtml skip normalAbstractCell
            else 
               -- TODO calculate factor foor either row or column
               if (modFactors selectionFactor.factor (cubeColumnSingeFactor cubeColumns) == 0) then 
                  cellHtml skip associatedAbstractCell
               else
                  cellHtml skip normalAbstractCell
         ValueRow cubeValueRow ->
               let
                  content = 
                     valueFetcher
                     |> getContentFromRowAndMembers cubeValueRow cubeColumns
               in
                  if skip then
                     content
                     |> normalCell cubeValueRow cubeColumns
                     |> cellHtml skip 
                  else 
                     let
                        cubeFactor = cubeColumnSingeFactor cubeColumns
                        totalFactor = 
                           cubeColumns 
                           |> factorsForCell cubeValueRow 
                           |> multiply 
                     in
                        if isFactorSelection selectionFactor.factor totalFactor then
                          content
                           |> selectedCell selectionFactor.selection cubeValueRow cubeColumns
                           |> cellHtml True
                        else
                           if (modFactors selectionFactor.factor (cubeRow |> cubeValueRowFactors |> multiply) == 0) || (modFactors selectionFactor.factor cubeFactor == 0) then
                              content
                              |> associatedCell cubeValueRow cubeColumns
                              |> cellHtml skip 
                           else
                              content
                              |> normalCell cubeValueRow cubeColumns
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
            |> normalCell cubeRow cubeColumn
            |> cellHtml False 

type RowIndex = RowIndex Int
type ColumnIndex = ColumnIndex Int

createGridCell: ValueFetcher -> Bool -> Maybe SelectionWithFactor -> KonceptRow -> CubeColumn -> CellHtml
createGridCell valueFecher skip maybeSelectionWithFactor konceptRow cubeColumns =
   case maybeSelectionWithFactor of
      Just selection ->  gridCellWithSelection valueFecher skip selection  konceptRow cubeColumns
      Nothing -> gridCellWithoutSelection valueFecher konceptRow cubeColumns

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


cell: ValueFetcher -> Direction -> Area -> Maybe SelectionWithFactor -> ColumnIndex -> CubeColumn -> RowIndex -> KonceptRow -> (Bool,List (Html Msg)) -> (Bool,List (Html Msg))
cell valueFetcher direction area maybeSelection columnIndex column rowIndex row state  =
   let
      attr =
         cellAreaAttributes direction area columnIndex rowIndex
         |> addAttr attrCell
         |> addAttr attrBox

      htmlCell =  createGridCell valueFetcher (first state) maybeSelection row column        
   in  
      (htmlCell.isSelected, [ creatorCreateCell htmlCell.html attr ] ++ (second state))


gridCells: ValueFetcher -> Direction -> Maybe Selection -> CubeColumns -> CubeRows -> List (Html Msg)
gridCells valueFetcher direction selection cubeColumns cubeRows =
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
                   |> Lists.foldi (\i state row-> cell valueFetcher direction area selectionWithFactors columnIndex cubeColumn (RowIndex (i + 1)) row state) (selected,[])
              
            in
               (first newState, [ second newState ] ++ (second acc))
              
      in
         --- fold over columns
         cubeColumns.columns
         |> Lists.foldi (\i state col -> (cells cubeRows.rows) (ColumnIndex (i + 1)) col state) (False,[])
         |> (\(_,result) -> result)
         |> List.concat


viewCube: ValueFetcher -> Direction -> HyperCube -> List DimensionalKoncept -> Maybe Selection -> Html Msg
viewCube valueFetcher direction hyperCube koncepts selection =
    let 

         factorsForSelection: List Factor
         factorsForSelection = 
            case selection of
               Just s -> 
                 factorsInSelection s
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
            |> calculateCubeColumns direction selection
       
         columns: List (Html Msg) 
         columns = 
            cubeColumns.headers 
            |> List.map (\header -> columnHeaderCell direction cubeRows.offset header)

         cells: List (Html Msg) 
         cells = 
            gridCells valueFetcher direction selection cubeColumns cubeRows

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

