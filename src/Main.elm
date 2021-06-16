module Main exposing (main)

import Ball exposing (..)
import Base exposing (..)
import Brick exposing (..)
import Browser exposing (element)
import Browser.Events exposing (Visibility(..), onAnimationFrameDelta, onKeyDown, onKeyUp, onVisibilityChange)
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


type GameStatus
    = Play
    | Pause
    | Stop
    | WaitToContinue


type alias Model =
    { brick : List Brick
    , paddle : Paddle
    , brickSize : ( Int, Int )
    , ball : Ball
    , size : ( Float, Float )
    , keyPress : Dir
    , gameStatus : GameStatus
    , gamelife : Int
    , score : Int
    , money : Int
    }


initModel : Model
initModel =
    Model (initBricks 10 10) (initPaddle (Color 101 101 101) ( 175, 300 ) 80 5) ( 10, 10 ) initBall ( wholeWidth, wholeHeight ) NoDir Play 1 0 1


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
            (viewBrickList model.brick ++ viewBall model.ball ++ seePaddle model.paddle)
        , p [ HtmlAttr.hidden (not (checkPause model)) ] [ text "Press space to continue" ]
        , renderInfo model model.score 
        , renderScore model
        ]

renderInfo : Model -> Int -> Html Msg
renderInfo mod score =
    div
        -- [ style "background" "rgba(245,212,217, 0.7)"
        [  style "color" "pink"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "50px"
        , style "font-weight" "bold"
        , style "line-height" "10"
        , style "position" "absolute"
        , style "left" "370px"
        , style "top" "0"
        , style "width" "400px"
        , style "height" "400px"
        , style "display"
            (
             if mod.gamelife == 0 then
                "block"
             else "none"
            )
        ]
        [ 
            text ("Died! Score is " ++ (toString(score)))
        ]
renderScore : Model -> Html Msg
renderScore mod  =
    div
       
        [  style "color" "pink"
        , style "font-family" "Helvetica, Arial, sans-serif"
        , style "font-size" "10px"
        , style "font-weight" "bold"
        , style "line-height" "10"
        , style "position" "absolute"
        , style "left" "700px"
        , style "top" "0"
        , style "width" "400px"
        , style "height" "400px"
        , style "display"
            (
             if mod.gamelife /= 0 then
                "block"
             else "none"
            )
        ]
        [ 
            text ("Score: " ++ (toString(mod.score)) ++ " Money: " ++ (toString(mod.money)))
        ]

checkPause : Model -> Bool
checkPause a =
    if a.gameStatus == Pause then
        True

    else if a.gameStatus == WaitToContinue then
        True
    -- else if a.gamelife == 0 then
    --     True
    
    else
        False

-- checkDead : Model -> Bool
-- checkDead a =
--     if a.gamelife == 0 then
--         True
--     else False

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model |> checkBuy msg, Cmd.none )
        |> checkStatus msg
        |> gameRunUpdate msg
        

checkBuy : Msg -> Model ->  Model
checkBuy msg model = 
    case msg of 
        Buy1->
            checkBuy1 model
        Buy2->
            checkBuy2 model
        Buy3->
            checkBuy3 model
        _ ->
             model

checkBuy1 : Model -> Model
checkBuy1 model = 
    model

checkBuy2 : Model -> Model
checkBuy2 model =
    let 
      oldwidth = (firstPaddle model.paddle).width
      oldmoney = model.money
      olddpad = model.paddle
    in
    if  model.money >= 1  then
        {model | money = oldmoney - 1 , paddle = { olddpad | shape = List.map (\theserec -> {theserec| width = oldwidth + 20} ) olddpad.shape } }
    else model 

checkBuy3 : Model -> Model
checkBuy3 model =
    model

gameRunUpdate : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
gameRunUpdate msg ( model, cmd ) =
    case model.gameStatus of
        Play ->
            ( model, cmd )
                |> updateKey msg
                |> updateBall msg
                |> checkBallMeet msg
                |> updatePaddle msg
                -- |> checkPosition msg

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

        Continue ->
            if model.gameStatus == WaitToContinue then
                ( { model | gameStatus = Play }, cmd )

            else
                ( model, cmd )

        _ ->
            ( model, cmd )


-- checkPosition : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
-- checkPosition msg ( model, cmd ) =
--     case msg of
--         Tick time ->
--             ( { model | paddle = checkPaddlePosition model.paddle model.size }, cmd )

--         _ ->
--             ( model, cmd )


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
            ( { model | paddle = movePaddle model.paddle model.keyPress |> updateBothpaddle }, cmd )

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
                oldmoney = model.money
                oldscore = model.score
                oldbricks = List.length model.brick
                newbricks = 
                    List.filter
                        (\a ->
                            case deleteBrick nowball3.shape a of
                                Nothing ->
                                    True

                                Just ( c, d ) ->
                                    False
                        )
                        model.brick
                nowball1 =
                    (model.ball |> ballRotate) (checkBallBrickMeet model.ball model.brick)

                nowball11 =
                    (nowball1 |> ballRotate) (checkBallPaddle1Meet nowball1 model.paddle)

                nowball2=
                    (nowball11 |> ballRotate) (checkBallPaddle2Meet nowball11 model.paddle)
                nowball3 =
                    (nowball2 |> ballRotate) (Tuple.second (checkBallBorderMeet nowball2 model.size) )
                
                nowball4 =
                    (Tuple.first (checkBallBorderMeet nowball3 model.size) )
            in
            ( { model
                | ball = nowball4
                , brick =
                    newbricks
                 ,gamelife = nowball4.life
                 ,score = oldscore + oldbricks - (List.length newbricks)
                 ,money = oldmoney + oldbricks - (List.length newbricks)
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
        38 ->
            KeyPress Up

        40 ->
            KeyPress Down 

        37 ->
            KeyPress Left

        39 ->
            KeyPress Right

        32 ->
            Continue
        
        97->
            Buy1
        
        98->
            Buy2

        99->
            Buy3

        _ ->
            KeyPress NoDir


keyRelease : Int -> Msg
keyRelease i =
    case i of
        37 ->
            KeyRelease Left

        39 ->
            KeyRelease Right
        
        38 ->
            KeyPress Up

        40 ->
            KeyPress Down 

        _ ->
            KeyRelease NoDir

