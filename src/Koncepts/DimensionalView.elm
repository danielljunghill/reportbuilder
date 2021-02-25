module Koncepts.DimensionalView exposing (..)
import Koncepts.Model exposing (..)
import Koncepts.DimensionalHeader exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Lines exposing (..)
import NList exposing (..)
import NList
import Msg exposing (..)
import Msg 
import Html exposing (..)
import Html.Attributes exposing (..)

type Rows = Rows Int
type Columns = Columns Int

type Row = Row Int
type Column = Column Int
-- concatStyles: List String -> String
-- concatStyles styles =
--     let
--         separator: String
--         separator = ";"
--         recConcat: List String -> String
--         recConcat s =
--             case s of
--                 [] -> ""
--                 [ head ] -> head
--                 head :: tail -> head ++ ";" ++ recConcat tail
--     in
--         recConcat styles
createAttributeValue: String -> String -> Int -> String -> Attribute msg
createAttributeValue s1 s2 i s3 =
   i 
   |> String.fromInt
   |> (\s -> s2 ++ s ++ s3)
   |> style s1

createGrid: Rows -> Columns -> List (Attribute msg) 
createGrid (Rows rows) (Columns columns) =
    let 
      
        attrdisplay: Attribute msg
        attrdisplay = style "display" "grid"  

        attrColumns: Attribute msg
        attrColumns = createAttributeValue "grid-template-columns" "repeat(" columns ")"

        attrRows: Attribute msg
        attrRows = createAttributeValue "grid-template-rows" "repeat(" rows ", minmax(50px,auto))"

    in
     [ attrdisplay , attrRows , attrColumns]

gridPointAttr: Row -> Column -> List (Attribute msg)
gridPointAttr row column =
    let 
        gridItemAttr: String -> Int ->  Attribute msg
        gridItemAttr s i =
            String.fromInt i
            |> style s
        startRow: Row -> Attribute msg   
        startRow (Row r) = gridItemAttr "grid-row-start" r 
        startColumn: Column -> Attribute msg   
        startColumn (Column c) = gridItemAttr "grid-column-start" c
    in
         [ column |> startColumn , row |> startRow ]
         
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

        tableHeaders: List TableHeader
        tableHeaders = 
            dimensions 
            |> calculateTableHeaders direction

        columns: DimensionColumns 
        columns = tableHeaders |> dimensionColumns 

        headers: List DimensionColumnHeader 
        headers = tableHeaders |> dimensionColumnHeaders 
        
    in
        div (createGrid (Rows 2) (Columns 2))  [

        ]



