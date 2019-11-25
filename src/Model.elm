-- MODEL


module Model exposing (Model, Msg(..), init, update)

import Cell exposing (Cell, CellStatus(..))
import Grid exposing (Grid)
import Random


type Status
    = Playing
    | Won
    | Lost


type alias Model =
    { grid : Maybe Grid
    , status : Status
    , buildWidth : Int
    , buildHeight : Int
    }


init : Model
init =
    { grid = Nothing
    , status = Playing
    , buildWidth = 10
    , buildHeight = 10
    }



-- UPDATE


type Msg
    = Create Int Int
    | SetWidth String
    | SetHeight String
    | NewGrid Grid
    | Click Int Int
    | Flag Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Create w h ->
            ( model, Random.generate NewGrid (Grid.createGrid w h) )

        SetWidth w ->
            ( { model
                | buildWidth = Maybe.withDefault 10 (String.toInt w)
              }
            , Cmd.none
            )

        SetHeight h ->
            ( { model
                | buildHeight = Maybe.withDefault 10 (String.toInt h)
              }
            , Cmd.none
            )

        NewGrid grid ->
            ( { model | grid = Just grid }, Cmd.none )

        Flag x y ->
            if model.status == Playing then
                case model.grid of
                    Just grid ->
                        ( { model | grid = Just (Grid.flagAt x y grid) }, Cmd.none )

                    Nothing ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        Click x y ->
            if model.status == Playing then
                case model.grid of
                    Just grid ->
                        let
                            newGrid =
                                Grid.digAt x y grid
                        in
                        ( { model
                            | grid = Just newGrid
                            , status =
                                if Grid.hasLost newGrid then
                                    Lost

                                else
                                    Playing
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( { model | grid = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )
