module Koncepts.Area exposing (..)

type ColumnSpan = ColumnSpan Int
intColumnSpan (ColumnSpan v) = v

type RowSpan = RowSpan Int
intRowSpan (RowSpan v) = v

type Row = Row Int
intRow (Row v) = v

type Column = Column Int
intColumn (Column v) = v

type Orientation =
    RowVerticalAxis
    | RowHorizontalAxis

type alias Area =
    {
            row: Row
        ,   rowSpan: RowSpan
        ,   column: Column
        ,   columnSpan: ColumnSpan
    }

addRow (Row row1) (Row row2) = 
    row1 + row2
    |> Row

addColumn (Column column1) (Column column2) = 
    column1 + column2
    |> Column

addRowSpan (RowSpan rowSpan1) (RowSpan rowSpan2) = 
    rowSpan1 + rowSpan2
    |> RowSpan

addColumnSpan (ColumnSpan columnSpan1) (ColumnSpan columnSpan2) = 
    columnSpan1 + columnSpan2
    |> ColumnSpan

substractColumnSpan (ColumnSpan columnSpan1) (ColumnSpan columnSpan2) =
    columnSpan1 - columnSpan2
    |> ColumnSpan

incColSpan = addColumnSpan (ColumnSpan 1)
  
incRowSpan = addRowSpan (RowSpan 1)

incRow = addRow (Row 1)

incColumn = addColumn (Column 1)

decColSpan = addColumnSpan (ColumnSpan -1)
  
decRowSpan = addRowSpan (RowSpan -1)

decRow = addRow (Row -1)

decColumn = addColumn (Column -1)

areaAllSize size =
    {
            row = Row size
        ,   rowSpan = RowSpan size
        ,   column = Column size
        ,   columnSpan = ColumnSpan size
    }

zeroArea = areaAllSize 0
oneArea = areaAllSize 1

type alias Offset =
   {
         row: Row
      ,  column: Column
   }

offsetAllSize size =
   {
         row = Row size
      ,  column = Column size
   }

zeroOffset = offsetAllSize 0
oneOffset = offsetAllSize 1


addRowToOffset: Row -> Offset -> Offset
addRowToOffset row offset = { offset | row = row |> addRow offset.row }

addColumnToOffset: Column -> Offset -> Offset
addColumnToOffset column offset = { offset | column = column |> addColumn offset.column }

addColumnToArea: Column -> Area -> Area
addColumnToArea column area = 
    { area | column = column |> addColumn area.column }

addRowToArea: Row -> Area -> Area
addRowToArea row area = 
    { area | row = row |> addRow area.row }


addRowSpanToArea: RowSpan -> Area -> Area
addRowSpanToArea span area = 
    { area | rowSpan = span |> addRowSpan area.rowSpan }


addColumnSpanToArea: ColumnSpan -> Area -> Area
addColumnSpanToArea span area = 
    { area | columnSpan = span |> addColumnSpan area.columnSpan }


offsetArea: Offset -> Area -> Area 
offsetArea offset area =
      area 
      |> addRowToArea offset.row
      |> addColumnToArea  offset.column

