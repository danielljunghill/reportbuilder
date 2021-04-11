module Koncepts.CubeView2 exposing (..)
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
import Koncepts.CubeModel exposing (..)
import Koncepts.CubeRow exposing (..)
import Koncepts.Factorhandling exposing(..)

tryGetfactors: CubeRow -> CubeColumn -> Maybe (NList Factor, List AbstractFactor)
tryGetfactors cubeRow (CubeColumn members) =
      case cubeRow.konceptPath of
      AbstractPath _ -> Nothing
      ValuePath kvp ->
         let 
            factors =
                cubeRow.members |> List.map (\m -> m.factor)
                |> NList.addList (NList.create kvp.value.factor)
                |> NList.append (membersFactorList members)
         in
            Just (factors, kvp.abstracts |> List.map (\ak -> ak.factor))

attrEventSelectCell: CubeColumn -> CubeRow -> List (Attribute Msg) -> List (Attribute Msg)
attrEventSelectCell cubeColumn cubeRow attr = 
         let

            event: List (Attribute Msg) 
            event = 
               tryGetfactors cubeRow cubeColumn
               |> Maybe.map (\(factors,abstract) -> (factors,abstract) |> SelectValue |> Msg.SelectMsg |> onClick)
               |> Lists.maybeAsList 
         in
            event ++ attr

attrEventEditCell: CubeColumn ->  CubeRow  -> List (Attribute Msg) -> List (Attribute Msg)
attrEventEditCell cubeColumn cubeRow attr = 
         let
            event:List (Attribute Msg)
            event = 
               tryGetfactors cubeRow cubeColumn
               |> Maybe.map (\(factors,abstract) -> (factors,abstract) |> EditValue |> Msg.SelectMsg |> onDoubleClick)
               |> Lists.maybeAsList
         in
            event ++ attr

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

attrRowHeaderAbstract: KonceptRow -> List (Attribute msg)
attrRowHeaderAbstract rowHeader = 
   case tryGetValueKoncept rowHeader.item of
      Nothing -> [ class "abstract-header" ]
      Just _ -> []

attrRowHeaderSelected  =  "grid-row-header-selected" |> createClassAttr
attrRowHeader = "grid-row-header" |> createClassAttr

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
         |> addAttr attrRowHeader
         |> addAttr (attrSelected2 attrRowHeaderSelected selected)
         |> addAttr (attrRowHeaderAbstract rowHeader)
         |> textCell (Content (konceptRowItemName rowHeader.item))


cubeColumnSingeFactor (CubeColumn members) =
   members
   |> membersFactor

rowAndMemberFactorList: ValueKoncept -> CubeColumn -> NList Factor
rowAndMemberFactorList vk (CubeColumn members) =
      hyperValueFactorList vk members 
 


-- selectionFactors: Selection -> NList Factor
-- selectionFactors selection =
--    case selection of
--       EditValue (edited -> edited
--       SelectValue selected -> selected

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
               cubeColumn
               |> rowAndMemberFactorList valueKoncept
               |> inputCell content 
               |> CellCreator
      SelectValue _ ->
               (\attr -> 
                  attrEventEditCell content cubeColumn valueKoncept attr
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


getContentFromRowAndMembers: ValueKoncept -> CubeColumn -> ValueFetcher -> Content 
getContentFromRowAndMembers valueKoncept (CubeColumn members) valueFetcher =
         members
         |> hyperValueFactorList valueKoncept   
         |> getContentValueFromFetcher valueFetcher

gridCellWithSelection: ValueFetcher -> Bool -> SelectionWithFactor -> KonceptRow -> CubeColumn -> CellHtml
gridCellWithSelection valueFetcher skip selectionFactor konceptRow cubeColumns =
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
               let
                  content = 
                     valueFetcher
                     |> getContentFromRowAndMembers row cubeColumns
               in
                  if skip then
                     content
                     |> normalCell row cubeColumns
                     |> cellHtml skip 
                  else 
                     let
                        cubeFactor = cubeColumnSingeFactor cubeColumns
                        totalFactor = multiply cubeFactor row.factor
                     in
                        if isFactorSelection selectionFactor.factor totalFactor then
                          content
                           |> selectedCell selectionFactor.selection row cubeColumns
                           |> cellHtml True
                        else
                           if (modFactors selectionFactor.factor row.factor == 0) || (modFactors selectionFactor.factor cubeFactor == 0) then
                              content
                              |> associatedCell row cubeColumns
                              |> cellHtml skip 
                           else
                              content
                              |> normalCell row cubeColumns
                              |> cellHtml skip 

gridCellWithoutSelection: ValueFetcher -> CubeRow -> CubeColumn -> CellHtml 
gridCellWithoutSelection valueFetcher cubeRow cubeColumn =
      case cubeRow.konceptPath of
         AbstractPath _ ->
            normalAbstractCell  
            |> cellHtml False 
         ValuePath row ->
            valueFetcher
            |> getContentFromRowAndMembers row cubeColumn
            |> normalCell row cubeColumn
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

