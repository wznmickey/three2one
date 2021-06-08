module Main exposing (main)

import Ball exposing (..)
import Base exposing (..)
import Brick exposing (..)
import Browser exposing (element)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Color exposing (..)
import Debug exposing (toString)
import For exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Meet exposing (..)
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
update msg model =
    ( model, Cmd.none )
        |> updateBall msg
        |> checkBallMeet msg


updateBall : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateBall msg ( model, cmd ) =
    case msg of
        Tick time ->
            ( { model | ball = ballRun model.ball time }, cmd )

        BallMeet dir ->
            ( { model | ball = ballRotate model.ball dir }, cmd )

        _ ->
            ( model, cmd )


checkBallMeet : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkBallMeet msg ( model, cmd ) =
    case msg of
        Tick time ->
            let
                nowball1 =
                    (model.ball |> ballRotate) (checkBallBrickMeet model.ball model.brick)

                nowball2 =
                    (nowball1 |> ballRotate) (checkBallPaddleMeet nowball1 model.paddle)

                nowball3 =
                    (nowball2 |> ballRotate) (checkBallBorderMeet nowball2 model.size)
            in
            ( { model | ball = nowball3 }, cmd )

        _ ->
            ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
