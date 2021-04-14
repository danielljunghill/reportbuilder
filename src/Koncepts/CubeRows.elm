module Koncepts.CubeRows exposing (..)
import Koncepts.CubeRow exposing (..)
import Koncepts.CubeRow as CubeRow
import Koncepts.CubeRowHeader as CubeRowHeader
import Model exposing (Selection(..))
import Koncepts.CubeModel exposing (..)
import Koncepts.Area exposing (..)
import Koncepts.Model exposing (DimensionalKoncept(..))

type CubeColumnOffset = CubeColumnOffset Offset

cubeColumnOffsetToOffset: CubeColumnOffset -> Offset
cubeColumnOffsetToOffset (CubeColumnOffset offset) = offset

type CubeRowHeader = CubeRowHeader CubeHeader
 
type alias CubeRows =
   {
         rows: List CubeRow
      ,  headers: List CubeRowHeader
      ,  offset: CubeColumnOffset 

   }

createCubeRowsIndented: Maybe Selection -> List DimensionalKoncept -> CubeRows
createCubeRowsIndented selection koncepts =
    {
            rows = CubeRow.createIndented koncepts
        ,   headers = 
                    koncepts
                    |> CubeRowHeader.createIndentedHeader selection 
                    |> List.map CubeRowHeader
        ,   offset = 
                Start 1 
                |> HorizontalStart 
                |> addHorizontalStartToOffset emptyOffset 
                |> CubeColumnOffset 
    }
