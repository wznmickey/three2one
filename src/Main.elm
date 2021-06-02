module Main exposing (main)

import Ball exposing (..)
import Brick exposing (..)
import Browser exposing (element)
import Color exposing (..)
import For exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Msg exposing (..)
import Paddle exposing (..)
import Rectangle exposing (..)
import String exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task exposing (..)


wholeHeight =
    400


wholeWidth =
    400


type alias Model =
    { brick : List Brick
    , paddle : Paddle
    , brickSize : ( Int, Int ) 
    , ball : Ball
    , size : ( Float, Float )
    }


initModel : Model
initModel =
    Model (initBricks 20 10) (initPaddle (Color 101 101 101) ( 175, 300 ) 50 5) ( 20, 10 ) initBall ( wholeHeight, wholeWidth )


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "width" (fromInt wholeWidth)
        , HtmlAttr.style "height" (fromInt wholeHeight)
        , HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "text-align" "center"
        ]
        [ Svg.svg
            [ SvgAttr.width (fromInt wholeWidth)
            , SvgAttr.height (fromInt wholeHeight) 
            ]
            (viewBrickList model.brick ++ viewBall model.ball ++ viewPaddle model.paddle)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update a b =
    ( b, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions a =
    Sub.batch []


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
