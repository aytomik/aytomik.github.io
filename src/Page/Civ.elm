module Page.Civ exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes as SvgAtt


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
    , size : ( Int, Int )
    }


type Msg
    = Move Id Coords


init : Model
init =
    { map = Dict.empty, size = ( 1084, 1920 ) }


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



-- RENDER


drawHex : Int -> Int -> Int -> Html Msg
drawHex x y r =
    let
        fx =
            toFloat x

        fy =
            toFloat y

        fr =
            toFloat r

        -- p1
        x1 =
            String.fromInt x

        y1 =
            String.fromInt (y + r)

        -- p2
        x2 =
            String.fromInt (round (fx + fr * sqrt 3 / 2))

        y2 =
            String.fromInt (round (fy + fr / 2))

        -- p3
        x3 =
            String.fromInt (round (fx + fr * sqrt 3 / 2))

        y3 =
            String.fromInt (round (fy - fr / 2))

        -- p4
        x4 =
            String.fromInt x

        y4 =
            String.fromInt (y - r)

        -- p5
        x5 =
            String.fromInt (round (fx - fr * sqrt 3 / 2))

        y5 =
            String.fromInt (round (fy - fr / 2))

        -- p5
        x6 =
            String.fromInt (round (fx - fr * sqrt 3 / 2))

        y6 =
            String.fromInt (round (fy + fr / 2))

        path =
            x1 ++ "," ++ y1 ++ " " ++ x2 ++ "," ++ y2 ++ " " ++ x3 ++ "," ++ y3 ++ " " ++ x4 ++ "," ++ y4 ++ " " ++ x5 ++ "," ++ y5 ++ " " ++ x6 ++ "," ++ y6
    in
    Svg.polygon [ SvgAtt.points path, SvgAtt.fill "white", SvgAtt.stroke "black" ] []


drawMap : Model -> Html Msg
drawMap model =
    let
        ( w, h ) =
            model.size
    in
    Svg.svg [ SvgAtt.width "100%", SvgAtt.height "100%" ]
        [ drawHex (500) (500) 40
        ]


view : Model -> Html Msg
view model =
    div
        [ style "padding" "10px"
        , style "width" "100%"
        , style "hight" "100%"
        ]
        [ drawMap model
        ]
