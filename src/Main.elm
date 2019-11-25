-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


module Main exposing (main, view)

import Array exposing (Array)
import Browser
import Cell exposing (Cell, CellStatus(..))
import Debug
import Grid exposing (Grid, remainingCount, unsafeNeighbourCount)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (custom, on, onClick, onInput)
import Json.Decode exposing (bool, field)
import Model exposing (Model, Msg(..), init, update)
import Platform.Cmd exposing (Cmd)
import Task



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- VIEW


colView : Grid -> Int -> Array Cell -> Html Msg
colView g x col =
    div
        [ style "flex" "0"
        , style "display" "flex"
        , style "flex-direction" "column"
        ]
        (Array.toList
            (Array.indexedMap
                (\y cell -> cellView (unsafeNeighbourCount x y g) x y cell)
                col
            )
        )


gridView : Grid -> Html Msg
gridView g =
    div []
        [ text ("Mine count " ++ String.fromInt (remainingCount g))
        , div [ style "display" "flex" ]
            (Array.toList
                (Array.indexedMap
                    (colView g)
                    g
                )
            )
        ]


cellBg : Cell -> String
cellBg c =
    case c.status of
        Fresh ->
            "darkgrey"

        Clicked ->
            if c.safe then
                "lightgrey"

            else
                "red"

        Flagged ->
            "darkgrey"


colorMap =
    Array.fromList
        [ "black"
        , "blue"
        , "green"
        , "red"
        , "darkblue"
        , "darkred"
        , "turquoise"
        , "black"
        , "darkgrey"
        ]


cellColor : Cell -> Int -> String
cellColor c u =
    case c.status of
        Flagged ->
            "yellow"

        _ ->
            if c.safe then
                Maybe.withDefault "black" (Array.get u colorMap)

            else
                "black"


cellText : Cell -> Int -> String
cellText c u =
    case c.status of
        Flagged ->
            "F"

        Clicked ->
            if c.safe then
                if u == 0 then
                    ""

                else
                    String.fromInt u

            else
                "O"

        Fresh ->
            ""


cellView : Int -> Int -> Int -> Cell -> Html Msg
cellView u x y c =
    div
        [ on "click"
            (Json.Decode.map
                (\meta ->
                    if meta then
                        Flag x y

                    else
                        Click x y
                )
                (field "metaKey" bool)
            )
        , custom
            "contextmenu"
            (Json.Decode.succeed
                { message = Flag x y
                , preventDefault = True
                , stopPropagation = False
                }
            )
        , style "background-color" (cellBg c)
        , style "color" (cellColor c u)
        , style "text-align" "center"
        , style "width" "20px"
        , style "height" "20px"
        , style "border" "1px solid black"
        , style "margin-left" "-1px"
        , style "margin-top" "-1px"
        ]
        [ text
            (cellText
                c
                u
            )
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ div
            []
            [ input
                [ style "display" "block"
                , onInput SetWidth
                , value (String.fromInt model.buildWidth)
                ]
                []
            , input
                [ style "display" "block"
                , onInput SetHeight
                , value (String.fromInt model.buildHeight)
                ]
                []
            , button
                [ style "display" "block"
                , onClick (Create model.buildWidth model.buildHeight)
                ]
                [ text "Build" ]
            , Maybe.withDefault
                (text "No grid")
                (Maybe.map gridView model.grid)
            ]
        ]
