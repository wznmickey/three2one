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
<<<<<<< HEAD
    , gamelife : Int
    , score : Int
    , money : Int
    }


initModel : Model
initModel =
    Model (initBricks 10 10) (initPaddle (Color 101 101 101) ( 175, 300 ) 80 5) ( 10, 10 ) initBall ( wholeWidth, wholeHeight ) NoDir Play 1 0 1
=======
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
>>>>>>> 275108592250cd55ff54a3d6d6a5403b3632327e


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
            (viewBrickList model.brick ++ viewBall model.ball ++ seePaddle model.paddle)
        , p [ HtmlAttr.hidden (not (checkPause model)) ] [ text "Press space to continue" ]
<<<<<<< HEAD
        , renderInfo model model.score 
        , renderScore model
=======
        
     --]--(if model.modelStatus==Inited then "1" else "2") ]
>>>>>>> 275108592250cd55ff54a3d6d6a5403b3632327e
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
<<<<<<< HEAD
    ( model |> checkBuy msg, Cmd.none )
=======
    ( model, Cmd.none )
        |> checkInit msg
        |> timeGet msg
>>>>>>> 275108592250cd55ff54a3d6d6a5403b3632327e
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
<<<<<<< HEAD
                -- |> checkPosition msg
=======
                |> checkPosition msg
                |> updateBrick msg
>>>>>>> 275108592250cd55ff54a3d6d6a5403b3632327e

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
        KeyPress Up ->
            ({model| ball = ballSpeedUp model.ball},cmd)
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
<<<<<<< HEAD
            let 
                oldmoney = model.money
                oldscore = model.score
                oldbricks = List.length model.brick
                newbricks = 
                    List.filter
=======
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
>>>>>>> 275108592250cd55ff54a3d6d6a5403b3632327e
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
<<<<<<< HEAD
            Continue
        
        97->
            Buy1
        
        98->
            Buy2

        99->
            Buy3
=======
            Game Continue

        83 -> 
            Game Play

        38 -> 
            KeyPress Up
>>>>>>> 275108592250cd55ff54a3d6d6a5403b3632327e

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

