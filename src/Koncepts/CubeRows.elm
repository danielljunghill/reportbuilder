module Koncepts.CubeRows exposing (..)
import Koncepts.CubeRow exposing (..)
import Koncepts.CubeRowIndent as CubeRowIndent
import Koncepts.CubeRowHeader as CubeRowHeader
import Koncepts.CubeRowHeader exposing(..)
import Model exposing (Selection(..))
import Koncepts.CubeModel exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Model exposing (DimensionalKoncept(..))

type CubeColumnOffset = CubeColumnOffset Offset

cubeColumnOffsetToOffset: CubeColumnOffset -> Offset
cubeColumnOffsetToOffset (CubeColumnOffset offset) = offset

type alias CubeRows =
   {
         rows: List CubeRow
      ,  headers: List CubeRowHeader
      ,  offset: CubeColumnOffset

   }

createCubeRowsIndented: Maybe Selection -> List DimensionalKoncept -> CubeRows
createCubeRowsIndented selection koncepts =
    {
            rows = CubeRowIndent.createIndented koncepts
        ,   headers =
                    koncepts
                    |> CubeRowHeader.createIndentedHeader selection
        ,   offset =            
                  zeroOffset
                  |> addColumnToOffset (Column 1)
                  |> CubeColumnOffset
    }
