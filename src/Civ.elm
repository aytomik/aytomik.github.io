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
import Time
import Url


hexRad : Float
hexRad =
    40



-- per second
fps : Float
fps =
    24


type TileType
    = Tree Int
      -- | Stone Int
    | Person Pathing



-- (x, y, z)


type alias Coords =
    ( Float, Float, Float )


type alias Pathing =
    { currentStep : Coords, nextStep : Coords, path : List Coords }


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
    , people : List Id
    }


type Msg
    = MoveAll
    | GotViewport Dom.Viewport
    | WindowResized Int Int
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | Tick Time.Posix


testMap : Map



-- testMap = Dict.empty


testMap =
    let
        pStart =
            ( 5, 1, 0 )

        testPath =
            createPath pStart ( -10, -2, 0 )

        testPathing : Pathing
        testPathing =
            case testPath of
                cStep :: ntep :: path ->
                    { currentStep = cStep, nextStep = ntep, path = path }

                _ ->
                    { currentStep = ( 1, 0, 0 ), nextStep = ( 0, 0, 0 ), path = testPath }
    in
    Dict.fromList
        [ ( 1, { tileType = Person testPathing, coords = pStart } )
        , ( 2, { tileType = Tree 3, coords = ( 0, 0, 0 ) } )
        , ( 3, { tileType = Tree 3, coords = ( 0, 1, 0 ) } )
        , ( 4, { tileType = Tree 3, coords = ( 0, 2, 0 ) } )
        ]


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { map = testMap, size = ( 1084, 1920 ), people = [ 1 ] }, Task.perform GotViewport Dom.getViewport )



-- UTILS


createXY : List Float -> List Float -> List ( Float, Float )
createXY xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs


fractionalModBy : Float -> Float -> Float
fractionalModBy modulus x =
    x - modulus * toFloat (floor (x / modulus))


hex2xy : Coords -> ( Float, Float )
hex2xy ( x, y, z ) =
    ( hexRad * sqrt 3 * (x + y / 2 - z / 2), hexRad * (y + z) * sqrt 3 * sqrt 3 / 2 )


createPath : Coords -> Coords -> List Coords
createPath s e =
    let
        ( sx, sy, sz ) =
            s

        ( ex, ey, ez ) =
            e

        dx =
            ex - sx

        dy =
            ey - sy

        dz =
            ez - sz

        md =
            List.foldl (\d m -> abs d |> Basics.max m) 0 [ dx, dy, dz ]

        step it st d =
            (toFloat it * d / md + st) |> floor |> toFloat
    in
    List.map (\it -> ( step it sx dx, step it sy dy, step it sz dz )) <| List.range 0 (floor md)


dispatch : msg -> Cmd msg
dispatch msg =
    Task.succeed msg |> Task.perform identity



-- UTILS


moveAll : Model -> Model
moveAll model =
    List.foldl (\id md -> move id md) model model.people


move : Id -> Model -> Model
move id model =
    let
        { map } =
            model

        mv crs { currentStep, nextStep, path } =
            let
                ( x, y, z ) =
                    crs

                ( sx, sy, sz ) =
                    currentStep

                ( ex, ey, ez ) =
                    nextStep

                _ =
                    Debug.log "crs" crs

                _ =
                    Debug.log "currentStep" currentStep

                _ =
                    Debug.log "nextStep" nextStep

                dx =
                    (ex - sx) / fps

                dy =
                    (ey - sy) / fps

                dz =
                    (ez - sz) / fps

                newCrs =
                    ( x + dx, y + dy, z + dz )

                isStepChange =
                    List.all ((>=) (1 / fps)) [ x + dx - ex, y + dy - ey, z + dz - ez ]

                newPathing : Pathing
                newPathing =
                    case path of
                        nStep :: nPath ->
                            { currentStep = nextStep, nextStep = nStep, path = nPath }

                        _ ->
                            { currentStep = nextStep, nextStep = nextStep, path = path }
            in
            if isStepChange then
                { model | map = Dict.update id (Maybe.map (\tile -> { tile | coords = newCrs, tileType = Person newPathing })) map }

            else
                { model | map = Dict.update id (Maybe.map (\tile -> { tile | coords = newCrs })) map }

        -- Dict.update id (Maybe.map (\_ -> newValue)) dict
    in
    case Dict.get id model.map of
        Just tile ->
            case tile.tileType of
                Person pathing ->
                    mv tile.coords pathing

                _ ->
                    model

        Nothing ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveAll ->
            ( moveAll model, Cmd.none )

        WindowResized w h ->
            ( { model | size = ( w, h ) }, Cmd.none )

        GotViewport viewport ->
            ( { model | size = ( round viewport.viewport.width, round viewport.viewport.height ) }, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )

        Tick _ ->
            ( model, dispatch MoveAll )



-- RENDER


drawHex : Float -> Float -> Float -> Html Msg
drawHex x y r =
    let
        -- p1
        x1 =
            String.fromFloat x

        y1 =
            String.fromFloat (y + r)

        -- p2
        x2 =
            String.fromFloat (x + r * sqrt 3 / 2)

        y2 =
            String.fromFloat (y + r / 2)

        -- p3
        x3 =
            String.fromFloat (x + r * sqrt 3 / 2)

        y3 =
            String.fromFloat (y - r / 2)

        -- p4
        x4 =
            String.fromFloat x

        y4 =
            String.fromFloat (y - r)

        -- p5
        x5 =
            String.fromFloat (x - r * sqrt 3 / 2)

        y5 =
            String.fromFloat (y - r / 2)

        -- p5
        x6 =
            String.fromFloat (x - r * sqrt 3 / 2)

        y6 =
            String.fromFloat (y + r / 2)

        path =
            x1 ++ "," ++ y1 ++ " " ++ x2 ++ "," ++ y2 ++ " " ++ x3 ++ "," ++ y3 ++ " " ++ x4 ++ "," ++ y4 ++ " " ++ x5 ++ "," ++ y5 ++ " " ++ x6 ++ "," ++ y6
    in
    Svg.polygon [ SvgAtt.points path, SvgAtt.fill "white", SvgAtt.stroke "black" ] []


drawGrid : Model -> Html Msg
drawGrid model =
    let
        ( wi, hi ) =
            model.size

        w =
            toFloat wi

        h =
            toFloat hi

        r =
            hexRad

        xoff =
            fractionalModBy (r * sqrt 3) (w / 2)

        -- xoff =
        --     0
        yoff =
            fractionalModBy (r * 3) (h / 2)

        -- yoff =
        --     0
        wn1 =
            ceiling (w / (r * sqrt 3))

        wn2 =
            wn1 + 1

        yn1 =
            1 + 2 * ceiling (h / (6 * r))

        yn2 =
            yn1 - 2

        x1 =
            List.map (\x -> xoff + toFloat x * sqrt 3 * r) (List.range -1 wn1)

        x2 =
            List.map (\x -> xoff + toFloat x * sqrt 3 * r + sqrt 3 * r / 2) (List.range -1 wn2)

        y1 =
            List.map (\y -> yoff + toFloat y * 3 * r) (List.range -1 yn1)

        y2 =
            List.map (\y -> yoff + toFloat y * 3 * r + 1.5 * r) (List.range -1 yn2)

        coords =
            List.concat
                [ createXY x1 y1
                , createXY x2 y2
                ]

        -- coords =
        --     [ ( 0, 0 ), ( w // 2, h // 2 ), ( w - 100, h - 100 ), ( 20, 20 ), ( 100, 0 ) ]
    in
    Svg.svg [ SvgAtt.width (String.fromFloat (w - 20)), SvgAtt.height (String.fromFloat (h - 50)) ]
        (List.map (\( x, y ) -> drawHex x y r) coords)


renderTile : ( Int, Int ) -> Tile -> Html Msg
renderTile ( wi, hi ) tile =
    let
        w =
            toFloat wi

        h =
            toFloat hi

        ( x1, y1 ) =
            hex2xy tile.coords

        -- xoff =
        --     floor (fractionalModBy (toFloat hexRad * sqrt 3) (toFloat w / 2) * -1)
        xoff =
            0

        -- yoff =
        --     floor (fractionalModBy (toFloat hexRad * 3) (toFloat h / 2) * -1)
        yoff =
            0

        x =
            xoff + x1 + w / 2

        y =
            yoff + y1 + h / 2
    in
    case tile.tileType of
        Tree _ ->
            Svg.circle [ SvgAtt.cx (String.fromFloat x), SvgAtt.cy (String.fromFloat y), SvgAtt.r "20", SvgAtt.fill "green" ] []

        Person _ ->
            Svg.circle [ SvgAtt.cx (String.fromFloat x), SvgAtt.cy (String.fromFloat y), SvgAtt.r "20", SvgAtt.fill "#ff4500" ] []


drawTiles : Model -> Html Msg
drawTiles model =
    let
        ( w, h ) =
            model.size
    in
    Svg.svg [ SvgAtt.width (String.fromInt (w - 20)), SvgAtt.height (String.fromInt (h - 50)) ]
        (List.map (\tile -> renderTile model.size tile) (Dict.values model.map))


drawMap : Model -> Html Msg
drawMap model =
    let
        ( w, h ) =
            model.size
    in
    Svg.svg [ SvgAtt.width (String.fromInt (w - 20)), SvgAtt.height (String.fromInt (h - 50)) ]
        [ drawGrid model
        , drawTiles model
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
    Sub.batch
        [ Browser.Events.onResize WindowResized
        , Time.every (1000 / fps) (\time -> Tick time)
        ]



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
