module Page.Civ exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (svg)


type TileType
    = Tree Int
    | Stone Int
    | Person


type alias Coords =
    { x : Float, y : Float, z : Float }


type alias Tile =
    { tileType : TileType
    , coords : Coords
    }


type alias Id =
    Int


type alias Map =
    Dict Id Tile


type alias Model =
    { map : Map
    }


type Msg
    = Move Id Coords


init : Model
init =
    { map = Dict.empty }


move : Id -> Coords -> Model -> Model
move id dcor model =
    let
        newModel =
            model
    in
    newModel


update : Msg -> Model -> Model
update msg model =
    case msg of
        Move id dst ->
            move id dst model


view : Model -> Html Msg
view model =
    div []
        [ text "fucking separate civ page shit shit shit"
        ]
