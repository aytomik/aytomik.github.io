module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    {}


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


-- view : Model -> Html Msg
-- view model =
--     div []
--         [ text "fuck you"
--         ]

view : Model -> Html Msg
view model =
    div []
        [ text "the aytomik home page BABY"
        , ul []
            [ viewLink "/home"
            , viewLink "/civ"
            ]
        ]

viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
