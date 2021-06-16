module Main exposing (main)

import Ball exposing (..)
import Brick exposing (..)
import Browser exposing (element)
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
import Browser.Navigation exposing (Key)
import Color exposing (..)
import Debug exposing (toString)
import Dir exposing (..)
import For exposing (..)
import Html exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Html.Events exposing (keyCode)
import Json.Decode as Decode
import Meet exposing (..)
import Msg exposing (..)
import Paddle exposing (..)
import Random exposing (..)
import Rectangle exposing (..)
import String exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr
import Task exposing (..)
import Time exposing (..)
import Global exposing(..)




type ModelStatus
    = Uninited
    | Inited


type alias Model =
    { modelStatus : ModelStatus
    , brick : List Brick
    , paddle : Paddle
    , brickSize : ( Int, Int )
    , ball : Ball
    , size : ( Float, Float )
    , keyPress : Dir
    , gameStatus : GameStatus
    , seed : Seed
    }


initModel : Seed -> ModelStatus-> Model
initModel s status  =
    let
        ( brick, newSeed1 ) =
            initBricks 5 20 s

        paddle =
            initPaddle (Color 101 101 101) ( 175, 300 ) 80 5

        brickSize =
            ( 10, 10 )

        ball =
            initBall

        size =
            ( wholeWidth, wholeHeight )
        
        key_Press = --avoid name conflict with function `keyPress`
            NoDir

        gameStatus =
            if (status==Inited) then Play
            else Pause

        seed =
            newSeed1
    in
    Model status brick paddle brickSize ball size key_Press gameStatus seed


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel (initialSeed 0) Uninited , Cmd.batch [ perform GetTime now , Cmd.map (\_ -> Init) Cmd.none ] )


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
        , p [ HtmlAttr.hidden (not (checkPause model)) ] [ text "Press space to continue" ]
        
     --]--(if model.modelStatus==Inited then "1" else "2") ]
        ]


checkPause : Model -> Bool
checkPause a =
    if a.gameStatus == Pause then
        True

    else if a.gameStatus == WaitToContinue then
        True

    else
        False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> checkInit msg
        |> timeGet msg
        |> checkStatus msg
        |> gameRunUpdate msg


checkInit : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkInit msg ( model, cmd ) =
    case msg of
        Init ->
            ( initModel model.seed Inited, cmd )
        Game Play -> 
            ( initModel model.seed Inited, cmd )
      
        _ ->
            ( model, cmd )


timeGet : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
timeGet msg ( model, cmd ) =
    case msg of
        GetTime time ->
            ( { model | seed = initialSeed (Time.posixToMillis time) }, cmd )

        _ ->
            ( model, cmd )


gameRunUpdate : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
gameRunUpdate msg ( model, cmd ) =
    case model.gameStatus of
        Play ->
            ( model, cmd )
                |> updateKey msg
                |> updateBall msg
                |> checkBallMeet msg
                |> updatePaddle msg
                |> checkPosition msg
                |> updateBrick msg

        _ ->
            ( model, cmd )

updateBrick : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateBrick  msg ( model, cmd ) =
     case msg of
        Tick time ->
            ( { model | brick = updateBricks model.brick }, cmd )
        _ ->
            ( model, cmd )

checkStatus : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkStatus msg ( model, cmd ) =
    case msg of
        OnView x ->
            if x == Hidden then
                ( { model | gameStatus = Pause }, cmd )

            else if model.gameStatus == Pause then
                ( { model | gameStatus = WaitToContinue }, cmd )

            else
                ( model, cmd )

        Game Continue ->
            if model.gameStatus == WaitToContinue then
                ( { model | gameStatus = Play }, cmd )

            else
                ( model, cmd )

        _ ->
            ( model, cmd )


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
        KeyPress Up ->
            ({model| ball = ballSpeedUp model.ball},cmd)
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
            if x /= Left && x/=Right then
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
            ( { model
                | ball = nowball3
                , brick =
                    List.map
                        (\a ->
                            case deleteBrick nowball3.shape a of
                                Nothing ->
                                        a
                                    --{a|show="hidden"}

                                Just ( c, d ) ->
                                    --a
                                    {a|show="hidden"}
                        )
                        model.brick
              }
            , cmd
            )

        _ ->
            ( model, cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp (Decode.map keyRelease keyCode)
        , onKeyDown (Decode.map keyPress keyCode)
        , onVisibilityChange OnView
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

        32 ->
            Game Continue

        83 -> 
            Game Play

        38 -> 
            KeyPress Up

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
