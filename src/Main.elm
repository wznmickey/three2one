module Main exposing (..)

import For exposing (..)


type alias Brick =
    { location : ( Int, Int )
    , color : ( Int, Int, Int )
    }


type alias Paddle =
    { length : Int
    , leftLocation : Float
    }


type alias Model =
    { brick : List Brick
    , paddle : Paddle
    }


type Demo
    = Demo1
    | Demo2


type alias Msg =
    { init : Demo
    }



--main =
--    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
--init : () -> ( Model, Cmd Msg )
--init a =
--    ( initModel, Cmd.none )


setBricks : Int -> Int -> List Brick -> List Brick
setBricks a b l =
    l ++ [ Brick ( a, b ) ( 255, 255, 255 ) ]


initBricks : Int -> Int -> List Brick
initBricks a b =
    for_nonconstant 0 a (for_constant 0 b setBricks) []


initPaddle : Int -> Float -> Paddle
initPaddle a b =
    Paddle a b


initModel : Model
initModel =
    Model (initBricks 10 5) (initPaddle 5 10)


init : () -> ( Model, Cmd Msg )
init a =
    ( initModel, Cmd.none )
