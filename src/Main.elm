module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Civ as Civ
import Page.Home as Home
import Task
import Url
import Url.Parser as UrlPar



-- ROUTE


type Route
    = Home
    | CivPage


routeParser : UrlPar.Parser (Route -> a) a
routeParser =
    UrlPar.oneOf
        [ UrlPar.map Home UrlPar.top
        , UrlPar.map CivPage (UrlPar.s "civ")
        ]



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , civPage : Civ.Model
    , homePage : Home.Model
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { key = key
      , url = url
      , civPage = Civ.init
      , homePage = Home.init
      }
    , Task.perform GotViewport Dom.getViewport
    )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CivPageMsg Civ.Msg
    | HomePageMsg Home.Msg
    | WindowResized Int Int
    | GotViewport Dom.Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        CivPageMsg subMsg ->
            let
                newCivModel =
                    Civ.update subMsg model.civPage
            in
            ( { model | civPage = newCivModel }, Cmd.none )

        HomePageMsg subMsg ->
            let
                newHomeModel =
                    Home.update subMsg model.homePage
            in
            ( { model | homePage = newHomeModel }, Cmd.none )

        WindowResized w h ->
            let
                civPage =
                    model.civPage
            in
            ( { model | civPage = { civPage | size = ( w, h ) } }, Cmd.none )

        GotViewport viewport ->
            let
                civPage =
                    model.civPage
            in
            ( { model | civPage = { civPage | size = ( round viewport.viewport.width, round viewport.viewport.height ) } }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize WindowResized



-- VIEW


homePage : Model -> Browser.Document Msg
homePage model =
    { title = "aytomik"
    , body =
        [ text "the aytomik home page BABY"
        , ul []
            [ viewLink "/home"
            , viewLink "/civ"
            ]
        ]
    }


getContent : Model -> Html Msg
getContent model =
    case UrlPar.parse routeParser model.url of
        Nothing ->
            Home.view model.homePage |> Html.map HomePageMsg

        Just CivPage ->
            Civ.view model.civPage |> Html.map CivPageMsg

        Just Home ->
            Home.view model.homePage |> Html.map HomePageMsg


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
            [ a [ href "/home" ] [ text "fuck you" ]
            , text "nothing to look for here, get fucking lost"
            ]
        , getContent model
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]



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
