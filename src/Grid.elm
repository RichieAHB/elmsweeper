module Grid exposing
    ( Grid
    , createGrid
    , digAt
    , flagAt
    , getNeighbourDetails
    , hasLost
    , remainingCount
    , shouldExpandNeighbours
    , unsafeNeighbourCount
    )

import Array exposing (Array)
import Cell exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Random


type alias Grid =
    Array (Array Cell)


createGrid : Int -> Int -> Random.Generator Grid
createGrid w h =
    Random.map
        Array.fromList
        (Random.list w (Random.map Array.fromList (Random.list h Cell.createCell)))


update : (a -> a) -> Int -> Array a -> Array a
update f i l =
    Maybe.withDefault l (Maybe.map (\a -> Array.set i (f a) l) (Array.get i l))


updateAt : (Cell -> Cell) -> Int -> Int -> Grid -> Grid
updateAt f x y g =
    update (update f y) x g


handleClick : (Cell -> Bool) -> (Cell -> Cell) -> Int -> Int -> Grid -> Grid
handleClick p f x y g =
    let
        maybeCell =
            cellAt x y g
    in
    case maybeCell of
        Just cell ->
            if p cell then
                updateAt f x y g

            else
                g

        Nothing ->
            g


flagAt : Int -> Int -> Grid -> Grid
flagAt x y g =
    handleClick (not << isClicked) toggleFlag x y g


digAt : Int -> Int -> Grid -> Grid
digAt x y g =
    let
        new =
            handleClick (not << isClicked) click x y g
    in
    if shouldExpandNeighbours x y new then
        List.foldl
            (\( _, xx, yy ) acc -> digAt xx yy acc)
            new
            (freshNeighbours x y new)

    else
        new


all : Grid -> List Cell
all g =
    List.concatMap Array.toList (Array.toList g)


hasLost : Grid -> Bool
hasLost g =
    List.any (\c -> isClicked c && (c |> (not << isSafe))) (all g)


countWhere : (Cell -> Bool) -> Grid -> Int
countWhere f g =
    List.sum
        (List.map
            (\cell ->
                if f cell then
                    1

                else
                    0
            )
            (all g)
        )


unsafeCount : Grid -> Int
unsafeCount =
    countWhere (not << isSafe)


flaggedCount : Grid -> Int
flaggedCount =
    countWhere isFlagged


remainingCount g =
    unsafeCount g - flaggedCount g


cellAt : Int -> Int -> Grid -> Maybe Cell
cellAt x y g =
    Maybe.andThen
        (Array.get y)
        (Array.get x g)


cellFromDetails : ( Cell, Int, Int ) -> Cell
cellFromDetails ( cell, _, _ ) =
    cell


getNeighbourDetails : Int -> Int -> Grid -> List ( Cell, Int, Int )
getNeighbourDetails x y g =
    List.filterMap identity
        (List.map
            (\( xo, yo ) ->
                let
                    xx =
                        x + xo

                    yy =
                        y + yo
                in
                Maybe.map (\c -> ( c, xx, yy )) (cellAt xx yy g)
            )
            [ ( -1, -1 )
            , ( 0, -1 )
            , ( 1, -1 )
            , ( -1, 0 )
            , ( 1, 0 )
            , ( -1, 1 )
            , ( 0, 1 )
            , ( 1, 1 )
            ]
        )


freshNeighbours : Int -> Int -> Grid -> List ( Cell, Int, Int )
freshNeighbours x y g =
    List.filter (cellFromDetails >> isFresh) (getNeighbourDetails x y g)


unsafeNeighbourCount : Int -> Int -> Grid -> Int
unsafeNeighbourCount x y g =
    List.length (List.filter (cellFromDetails >> (not << isSafe)) (getNeighbourDetails x y g))


shouldExpandNeighbours : Int -> Int -> Grid -> Bool
shouldExpandNeighbours x y g =
    Maybe.withDefault False (Maybe.map isSafe (cellAt x y g))
        && unsafeNeighbourCount x y g
        == 0
