module Koncepts.Area exposing (..)

type ColumnSpan = ColumnSpan Int
type Rowspan = RowSpan Int
type Row = Row Int
type Column = Column Int
type Orientation =
    | Horizontal
    | Vertical

type Area =
    {
            row: Row
        ,   rowSpan: RowSpan
        ,   col: Column
        ,   colSpan: ColumnSpan
    }

addRow (Row row1) (Row row1) = 
    row1 + row2
    |> Row

addColumn (Column column1) (Column column2) = 
    column1 + column2
    |> Row

addRowSpan (RowSpan rowSpan1) (RowSpan rowSpan2) = 
    rowSpan1 + rowSpan2
    |> RowSpan

addColumnSpan (ColumnSpan columnSpan1) (ColumnSpan columnSpan2) = 
    columnSpan1 + columnSpan2
    |> ColumnSpan

incColSpan = addColumnSpan (ColSpan 1)
  
incRowSpan = addRowSpan (RowSpan 1)

incRow = addRow (Row 1)

incColumn = addColumn (Column 1)

decColSpan = addColumnSpan (ColSpan -1)
  
decRowSpan = addRowSpan (RowSpan -1)

decRow = addRow (Row -1)

decColumn = addColumn (Column -1)

areaAllSize size =
    {
            row = Row size
        ,   rowSpan = RowSpan size
        ,   col = Column size
        ,   colSpan = ColumnSpan size
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

addColumnToOffset Column -> Area -> Area
addColumnToOffset column offset = { offset | column = column |> addColumn offset.column }

addColumnToArea: Column -> Area -> Area
addColumnToArea column area = 
    { area | column = column |> addColumn area.column }

addRowToArea: Row -> Area -> Area
addRowToArea row area = 
    { area | row = row |> addRow area.row }


addRowSpanToArea: RowSpan -> Area -> Area
addRowToArea span area = 
    { area | rowSpan = rowSpan |> addRowSpan area.rowSpan }


addRowToArea: ColumnSpan -> Area -> Area
addRowToArea span area = 
    { area | colSpan = colSpan |> addColumnSpan area.colSpan }


offsetArea: Offset -> Area -> Area 
offsetArea offset area =
      area
      |> addRowToArea offset.row
      |> addColumnToArea  offset.column

