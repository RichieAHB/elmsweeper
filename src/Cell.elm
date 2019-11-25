module Cell exposing (..)

import Array
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Random


type CellStatus
    = Clicked
    | Flagged
    | Fresh


type alias Cell =
    { safe : Bool, status : CellStatus }


createCell : Random.Generator Cell
createCell =
    Random.map
        (\safe -> { safe = safe, status = Fresh })
        (Random.weighted ( 0.1, False ) [ ( 0.9, True ) ])


isClicked : Cell -> Bool
isClicked c =
    c.status == Clicked


isFlagged : Cell -> Bool
isFlagged c =
    c.status == Flagged


isFresh : Cell -> Bool
isFresh c =
    c.status == Fresh


isSafe : Cell -> Bool
isSafe c =
    c.safe


click : Cell -> Cell
click c =
    { c | status = Clicked }


toggleFlag : Cell -> Cell
toggleFlag c =
    { c
        | status =
            if isFlagged c then
                Fresh

            else
                Flagged
    }
