module Main exposing (..)

import Browser
import Browser.Dom as Dom
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url



-- MODEL


type alias Model =
    { test : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( { test = "test"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UrlChanged _ ->
            ( model, Cmd.none )

        LinkClicked _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


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
        , text "why are you still here?"
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
