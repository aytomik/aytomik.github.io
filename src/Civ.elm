module Civ exposing (Model, Msg, init, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg
import Svg.Attributes as SvgAtt
import Task
import Url


hexRad =
    40


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
    | GotViewport Dom.Viewport
    | WindowResized Int Int
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { map = Dict.empty, size = ( 1084, 1920 ) }, Task.perform GotViewport Dom.getViewport )


createXY : List Int -> List Int -> List ( Int, Int )
createXY xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs


move : Id -> Coords -> Model -> Model
move id dcor model =
    let
        newModel =
            model
    in
    newModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move id dst ->
            ( move id dst model, Cmd.none )

        WindowResized w h ->
            ( { model | size = ( w, h ) }, Cmd.none )

        GotViewport viewport ->
            ( { model | size = ( round viewport.viewport.width, round viewport.viewport.height ) }, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )



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


drawGrid : Model -> Html Msg
drawGrid model =
    let
        ( w, h ) =
            model.size

        r =
            hexRad

        wf =
            toFloat w

        hf =
            toFloat h

        wn1 =
            ceiling (wf / (r * sqrt 3))
        wn2 = wn1 + 1

        yn1 =
            1 + 2 * ceiling (hf / (6 * r))
        yn2 = yn1 - 2

        x1 =
            List.map (\x -> floor (toFloat x * sqrt 3 * r)) (List.range 0 wn1)
        x2 =
            List.map (\x -> floor (toFloat x * sqrt 3 * r + sqrt 3 * r / 2)) (List.range 0 wn2)

        y1 =
            List.map (\y -> y * 3 * r) (List.range 0 yn1)
        y2 = 
            List.map (\y -> floor(toFloat y * 3 * r + 1.5 * r)) (List.range 0 yn2)

        coords =
          List.concat [
            createXY x1 y1,
            createXY x2 y2
            ]

        -- coords =
        --     [ ( 0, 0 ), ( w // 2, h // 2 ), ( w - 100, h - 100 ), ( 20, 20 ), ( 100, 0 ) ]
    in
    Svg.svg [ SvgAtt.width (String.fromInt (w - 20)), SvgAtt.height (String.fromInt (h - 50)) ]
        (List.map (\( x, y ) -> drawHex x y r) coords)


drawMap : Model -> Html Msg
drawMap model =
    let
        ( w, h ) =
            model.size
    in
    Svg.svg [ SvgAtt.width (String.fromInt (w - 20)), SvgAtt.height (String.fromInt (h - 50)) ]
        [ drawGrid model
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "aytomik"
    , body =
        [ div
            [ style "background-color" "orangered"
            , style "width" "100%"
            , style "height" "30px"
            , style "padding" "5px"
            , style "display" "flex"
            , style "flex-direction" "row"
            , style "gap" "10px"
            ]
            [ a [ href "/" ] [ text "fuck you" ]
            , text "nothing to look for here, get fucking lost"
            ]
        , div
            [ style "padding" "10px"
            , style "width" "100%"
            , style "hight" "100%"
            ]
            [ drawMap model
            ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowResized



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
