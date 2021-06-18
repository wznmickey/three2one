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
import Global exposing (..)
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
import Text exposing (..)
import Time exposing (..)


type ModelStatus
    = Uninited Int
    | Inited


type Location
    = OnGround
    | BelowGround
    | NoLocation


type alias Model =
    { modelStatus : ModelStatus
    , brick : List Brick
    , paddle : Paddle
    , ball : Ball
    , size : ( Float, Float )
    , keyPress : Dir
    , gameStatus : GameStatus
    , seed : Seed
    , score : Int
    , money : Int
    , text : ( String, Location )
    }



{--Init the model--}


initModel : Seed -> ModelStatus -> Model
initModel s status =
    let
        ( brick, newSeed1 ) =
            initBricks 5 10 s

        paddle =
            initPaddle (Color 101 101 101) ( 175, 275 ) 80 5

        ball =
            initBall

        size =
            ( wholeWidth, wholeHeight )

        key_Press =
            --avoid name conflict with function `keyPress`
            NoDir

        gameStatus =
            if status == Inited then
                Play

            else
                Pause

        seed =
            newSeed1

        score =
            0

        money =
            10

        text =
            ( "", NoLocation )
    in
    Model status brick paddle ball size key_Press gameStatus seed score money text



{--std init function--}


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel (initialSeed 0) (Uninited 1), Cmd.batch [ perform GetTime now, Cmd.map (\_ -> Init) Cmd.none ] )



{--std view function--}


view : Model -> Html Msg
view model =
    div
        [ HtmlAttr.style "left" "0"
        , HtmlAttr.style "top" "0"
        , HtmlAttr.style "text-align" "center"
        , HtmlAttr.style "background" "black"
        , HtmlAttr.style "width" "100vw"
        , HtmlAttr.style "height" "100vh"
        ]
        [ div
            [ HtmlAttr.style "width" (toString wholeWidth ++ "px")
            , HtmlAttr.style "border-width" "5px"
            , HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" "50%"
            , HtmlAttr.style "transform" "translate(-50%,-50%)"
            , HtmlAttr.style "top" "40%"
            , HtmlAttr.style "border-style" "solid"
            , HtmlAttr.style "border-color" "pink"
            ]
            [ checkShowGame model ]
        , div
            [ HtmlAttr.style "position" "absolute"
            , HtmlAttr.style "left" "50%"
            , HtmlAttr.style "transform" "translate(-50%,-50%)"
            , HtmlAttr.style "top" "75%"
            ]
            [ p [ HtmlAttr.hidden (not (checkPause model)), HtmlAttr.style "color" "pink" ] [ text "Press Space to continue" ]
            , p [ HtmlAttr.hidden (not (checkWaitS model)), HtmlAttr.style "color" "pink" ] [ text "Press S to start" ]
            , p [ HtmlAttr.hidden (not (checkScore model)), HtmlAttr.style "color" "pink" ] [ text (Tuple.first model.text) ]
            , renderInfo model
            , renderProperty model
            ]
        ]



{--Check whether show skill introduction based on the score--}


checkScore : Model -> Bool
checkScore model =
    if model.score <= 50 && model.score >= 5 && model.money > 0 then
        True

    else
        False



{--Check whether show the game or other things--}


checkShowGame : Model -> Html Msg
checkShowGame model =
    let
        ( t, k ) =
            model.text
    in
    if k == OnGround then
        p [ HtmlAttr.style "color" "pink", HtmlAttr.style "white-space" "pre-line", HtmlAttr.style "font-size" "small" ] [ text (Tuple.first model.text) ]

    else if model.money > 0 then
        Svg.svg
            [ SvgAttr.width (fromInt wholeWidth)
            , SvgAttr.height (fromInt wholeHeight)
            , SvgAttr.stroke "pink"
            ]
            (viewBrickList model.brick ++ viewBall model.ball ++ viewPaddle model.paddle)

    else
        div
            [ style "color" "pink"
            , HtmlAttr.style "white-space" "pre-line"
            , style "display"
                (if model.money <= 0 then
                    "block"

                 else
                    "none"
                )
            ]
            [ text (getText (Game Stop))
            ]



{--Show score and money--}


renderInfo : Model -> Html Msg
renderInfo mod =
    div
        [ style "color" "pink"
        , HtmlAttr.style "font-size" "large"
        ]
        [ if mod.money <= 0 then
            text (" Score is " ++ toString mod.score)

          else
            text ("Score: " ++ toString mod.score ++ " Money: " ++ toString mod.money)
        ]



{--Show game related info --}


renderProperty : Model -> Html Msg
renderProperty mod =
    div
        [ style "color" "pink"
        , HtmlAttr.style "white-space" "pre-line"
        , style "display"
            (if mod.money > 0 && mod.modelStatus == Inited then
                "block"

             else
                "none"
            )
        ]
        [ text ("Speed: " ++ toString mod.paddle.speed ++ " Paddle Width: " ++ toString (firstPaddle mod.paddle).width ++ "\n" ++ controlIntr)
        ]



{--Check whether there is a pause--}


checkPause : Model -> Bool
checkPause a =
    let
        ( t, k ) =
            a.text
    in
    if t == "" && k == OnGround then
        False

    else if a.gameStatus == Pause then
        True

    else if a.gameStatus == WaitToContinue then
        True

    else
        False



{--Check whether there need to press S--}


checkWaitS : Model -> Bool
checkWaitS model =
    let
        ( t, k ) =
            model.text
    in
    if t == "" && k == OnGround then
        True

    else
        False



{--std update function--}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> checkBuy msg
        |> showText msg
        |> checkInit msg
        |> timeGet msg
        |> checkStatus msg
        |> gameRunUpdate msg



{--Check what text should be shown--}


showText : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
showText msg ( model, cmd ) =
    let
        ntext =
            case model.modelStatus of
                Uninited x ->
                    getText (Game (Story x))

                _ ->
                    Tuple.first model.text

        nlocation =
            case model.modelStatus of
                Uninited x ->
                    OnGround

                _ ->
                    Tuple.second model.text
    in
    ( { model | text = ( ntext, nlocation ) }, cmd )



{--Check whether is buying increasing speed--}


checkBuy1 : Model -> Model
checkBuy1 model =
    let
        oldspeed =
            model.paddle.speed

        oldmoney =
            model.money

        olddpad =
            model.paddle
    in
    if model.money >= 12 then
        { model | money = oldmoney - 10, paddle = { olddpad | speed = oldspeed + 1 } }

    else
        model



{--Check whether is buying enlarging paddle --}


checkBuy2 : Model -> Model
checkBuy2 model =
    let
        oldwidth =
            (firstPaddle model.paddle).width

        oldmoney =
            model.money

        olddpad =
            model.paddle
    in
    if model.money >= 22 then
        { model | money = oldmoney - 20, paddle = { olddpad | shape = List.map (\theserec -> { theserec | width = oldwidth + 10 }) olddpad.shape } }

    else
        model



{--Call buy check function--}


checkBuy : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkBuy msg ( model, cmd ) =
    if model.money == 1 then
        ( { model | money = 0 }, cmd )

    else
        case msg of
            Buy 1 ->
                ( checkBuy1 model, cmd )

            Buy 2 ->
                ( checkBuy2 model, cmd )

            _ ->
                ( model, cmd )



{--Check whether need to init the game--}


checkInit : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkInit msg ( model, cmd ) =
    case msg of
        Init ->
            ( initModel model.seed Inited, cmd )

        Game Play ->
            ( initModel model.seed Inited, cmd )

        _ ->
            ( model, cmd )



{--Get time to set seed--}


timeGet : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
timeGet msg ( model, cmd ) =
    case msg of
        GetTime time ->
            ( { model | seed = initialSeed (Time.posixToMillis time) }, cmd )

        _ ->
            ( model, cmd )



{--Call game running related functions--}


gameRunUpdate : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
gameRunUpdate msg ( model, cmd ) =
    case model.gameStatus of
        Play ->
            ( model, cmd )
                |> updateKey msg
                |> updateBall msg
                |> checkBallMeet msg
                |> updatePaddle msg
                |> updateBrick msg

        _ ->
            ( model, cmd )



{--Check whether need to update texts--}


updateText : Model -> Model
updateText model =
    if model.money > 0 then
        if model.score >= 5 && model.score <= 20 then
            { model | text = ( getText (Game (Skill 1)), BelowGround ) }

        else if model.score <= 35 && model.score >= 5 then
            { model | text = ( getText (Game (Skill 2)), BelowGround ) }

        else if model.score <= 50 && model.score >= 5 then
            { model | text = ( getText (Game (Skill 3)), BelowGround ) }

        else
            model

    else
        model



{--Update bricks--}


updateBrick : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateBrick msg ( model, cmd ) =
    case msg of
        Tick time ->
            ( { model | brick = updateBricks model.brick }, cmd )

        _ ->
            ( model, cmd )



{--Check game status --}


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
            case model.modelStatus of
                Uninited x ->
                    ( { model | modelStatus = Uninited (x + 1) }, cmd )

                _ ->
                    if model.gameStatus == WaitToContinue then
                        ( { model | gameStatus = Play }, cmd )

                    else
                        ( model, cmd )

        _ ->
            ( model, cmd )



{--Update balls--}


updateBall : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateBall msg ( model, cmd ) =
    case msg of
        Tick time ->
            ( { model | ball = ballRun model.ball time }, cmd )

        BallMeet dir ->
            ( { model | ball = ballRotate model.ball dir }, cmd )

        KeyPress Up ->
            ( { model | ball = ballSpeedUp model.ball, money = model.money - 1 }, cmd )

        KeyPress Down ->
            ( { model | ball = ballSpeedDown model.ball, money = model.money - 1 }, cmd )

        _ ->
            ( model, cmd )



{--Update paddle(s)--}


updatePaddle : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updatePaddle msg ( model, cmd ) =
    case msg of
        Tick time ->
            ( { model | paddle = movePaddle model.paddle model.keyPress |> updateBothpaddle }, cmd )

        _ ->
            ( model, cmd )



{--Update the key info--}


updateKey : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateKey msg ( model, cmd ) =
    case msg of
        KeyPress x ->
            if x /= Left && x /= Right then
                ( model, cmd )

            else if (x == Left) || (x == Right) then
                ( { model | keyPress = x }, cmd )

            else
                ( model, cmd )

        KeyRelease x ->
            if x == model.keyPress then
                ( { model | keyPress = NoDir }, cmd )

            else
                ( model, cmd )

        _ ->
            ( model, cmd )



{--Check the ball meeting--}


checkBallMeet : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkBallMeet msg ( model, cmd ) =
    case msg of
        Tick time ->
            let
                oldmoney =
                    model.money

                oldscore =
                    model.score

                newbricks =
                    List.map
                        (\a ->
                            case deleteBrick nowball3.shape a of
                                Nothing ->
                                    a

                                --{a|show="hidden"}
                                Just ( c, d ) ->
                                    --a
                                    { a | show = "hidden" }
                        )
                        model.brick

                checkMeetBricks =
                    List.map2
                        (\a b ->
                            if a.show == b.show then
                                0

                            else
                                1
                        )
                        newbricks
                        model.brick

                nowball1 =
                    (model.ball |> ballRotateWithBrick) (checkBallBrickMeet model.ball model.brick)

                nowball11 =
                    (nowball1 |> ballRotate) (checkBallPaddle1Meet nowball1 model.paddle)

                nowball2 =
                    (nowball11 |> ballRotate) (checkBallPaddle2Meet nowball11 model.paddle)

                ( nowball3, ds ) =
                    checkBallBorderMeet nowball2 model.size

                dm =
                    if ds == -1 then
                        -200

                    else
                        0

                nowball4 =
                    if ds == 0 then
                        nowball3

                    else
                        { nowball3 | shape = backBall nowball3.shape model.paddle, speed = ( 0.15, 0.15 ) }

                ngameStatus =
                    if oldmoney + List.sum checkMeetBricks * 5 + dm <= 1 then
                        Stop

                    else
                        model.gameStatus
            in
            if List.sum checkMeetBricks == 0 then
                ( { model
                    | ball = nowball4
                    , brick = newbricks
                    , gameStatus = ngameStatus
                    , score = oldscore + List.sum checkMeetBricks
                    , money = oldmoney + List.sum checkMeetBricks * 5 + dm
                  }
                , cmd
                )

            else
                ( { model
                    | ball = nowball4
                    , brick = newbricks
                    , gameStatus = ngameStatus
                    , score = oldscore + List.sum checkMeetBricks
                    , money = oldmoney + List.sum checkMeetBricks * 5 + dm
                  }
                    |> updateText
                , cmd
                )

        _ ->
            ( model, cmd )



{--std subscriptions function--}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Tick
        , onKeyUp (Decode.map keyRelease keyCode)
        , onKeyDown (Decode.map keyPress keyCode)
        , onVisibilityChange OnView
        ]



{--std main function--}


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



{--Transfer key press msg--}


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
            --Space
            Game Continue

        81 ->
            --Q
            Buy 1

        69 ->
            --E
            Buy 2

        83 ->
            --S
            Game Play

        _ ->
            KeyPress NoDir



{--Transfer key release msg--}


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
