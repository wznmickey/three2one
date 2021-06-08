module Main exposing (main)

import Ball exposing (..)
import Base exposing (..)
import Brick exposing (..)
import Browser exposing (element)
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyUp)
import Browser.Navigation exposing (Key)
import Color exposing (..)
import Debug exposing (toString)
import For exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Meet exposing (..)
import Msg exposing (..)
import Paddle exposing (..)
import Rectangle exposing (..)
import String exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task exposing (..)


wholeHeight =
    350


wholeWidth =
    400


type alias Model =
    { brick : List Brick
    , paddle : Paddle
    , brickSize : ( Int, Int )
    , ball : Ball
    , size : ( Float, Float )
    , keyPress : Dir
    }


initModel : Model
initModel =
    Model (initBricks 20 10) (initPaddle (Color 101 101 101) ( 175, 300 ) 80 5) ( 20, 10 ) initBall ( wholeWidth, wholeHeight ) NoDir


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
        |> updateKey msg
        |> updateBall msg
        |> checkBallMeet msg
        |> updatePaddle msg
        |> checkPosition msg


checkPosition : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkPosition msg ( model, cmd ) =
    case msg of
        Tick time ->
            ( { model | paddle = checkPaddlePosition model.paddle model.size }, cmd )

        _ ->
            ( model, cmd )


updateBall : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateBall msg ( model, cmd ) =
    case msg of
        Tick time ->
            ( { model | ball = ballRun model.ball time }, cmd )

        BallMeet dir ->
            ( { model | ball = ballRotate model.ball dir }, cmd )

        _ ->
            ( model, cmd )


updatePaddle : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePaddle msg ( model, cmd ) =
    case msg of
        Tick time ->
            ( { model | paddle = movePaddle model.paddle model.keyPress }, cmd )

        _ ->
            ( model, cmd )


updateKey : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateKey msg ( model, cmd ) =
    case msg of
        KeyPress x ->
            if x == NoDir then
                ( model, cmd )

            else
                ( { model | keyPress = x }, cmd )

        KeyRelease x ->
            if x == model.keyPress then
                ( { model | keyPress = NoDir }, cmd )

            else
                ( model, cmd )

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
        , onKeyUp (Decode.map keyRelease keyCode)
        , onKeyDown (Decode.map keyPress keyCode)
        ]


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


keyPress : Int -> Msg
keyPress i =
    case i of
        37 ->
            KeyPress Left

        39 ->
            KeyPress Right

        _ ->
            KeyPress NoDir


keyRelease : Int -> Msg
keyRelease i =
    case i of
        37 ->
            KeyRelease Left

        39 ->
            KeyRelease Right

        _ ->
            KeyRelease NoDir
