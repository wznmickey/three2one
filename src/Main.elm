module Main exposing (main)

import Browser exposing (element)
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onResize)
import Debug exposing (toString)
import For exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task exposing (..)
import Tuple


type alias Color =
    { red : Int
    , blue : Int
    , green : Int
    }


type alias Brick =
    { location : ( Int, Int )
    , color : Color
    }


type alias Paddle =
    { length : Int
    , leftLocation : Float
    }


type alias Model =
    { brick : List Brick
    , paddle : Paddle
    , brickSize : ( Int, Int )
    }


type Demo
    = Demo1
    | Demo2


type alias Msg =
    { init : Demo
    }


getcolor : Color -> String
getcolor color =
    let
        r =
            fromInt color.red

        g =
            fromInt color.green

        b =
            fromInt color.blue
    in
    "rgb(" ++ r ++ "," ++ g ++ "," ++ b ++ ")"


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


setBricks : Int -> Int -> List Brick -> List Brick
setBricks a b l =
    l ++ [ Brick ( a, b ) (Color 101 101 101) ]


initBricks : Int -> Int -> List Brick
initBricks a b =
    for_nonconstant 0 a (for_constant 0 b setBricks) []


initPaddle : Int -> Float -> Paddle
initPaddle a b =
    Paddle a b


initModel : Model
initModel =
    Model (initBricks 10 5) (initPaddle 5 10) ( 10, 5 )


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )


gridsizeX =
    20


gridsizeY =
    10


mapsize : ( Int, Int )
mapsize =
    ( 10, 5 )


viewGrid : Brick -> Svg Msg
viewGrid brick =
    let
        ( x, y ) =
            brick.location
    in
    Svg.rect
        [ SvgAttr.width (toString gridsizeX)
        , SvgAttr.height (toString gridsizeY)
        , SvgAttr.x (toString (x * gridsizeX))
        , SvgAttr.y (toString (y * gridsizeY))
        , SvgAttr.fill (getcolor brick.color)
        , SvgAttr.stroke "black"
        ]
        []


range2d : ( Int, Int ) -> List ( Int, Int )
range2d size =
    let
        rangex =
            List.range 0 (Tuple.first size - 1)

        rangey =
            List.range 0 (Tuple.second size - 1)

        line =
            \y -> List.map (\x -> Tuple.pair x y) rangex
    in
    List.map line rangey
        |> List.concat


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "width" (fromInt (Tuple.first model.brickSize * gridsizeX))
        , HtmlAttr.style "height" (fromInt (Tuple.second model.brickSize * gridsizeY))
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "text-align" "center"
        ]
        [ Svg.svg
            [ SvgAttr.width (fromInt (Tuple.first model.brickSize * gridsizeX))
            , SvgAttr.height (fromInt (Tuple.second model.brickSize * gridsizeY))
            ]
            (List.map viewGrid model.brick)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update a b =
    ( b, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions a =
    Sub.batch []
