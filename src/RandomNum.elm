module RandomNum exposing (..)

import Msg exposing (..)
import Random exposing (..)

{--Int random Generator--}
genInt : Int -> Int -> Generator Int
genInt a b =
    Random.int a b

{--Float random Generator--}

genFloat : Float -> Float -> Generator Float
genFloat a b =
    Random.float a b

{--Get random Int--}

getRandomInt : Int -> Int -> Seed -> ( Int, Seed )
getRandomInt a b s =
    step (genInt a b) s

{--Get random Float--}

getRandomFloat : Float -> Float -> Seed -> ( Float, Seed )
getRandomFloat a b s =
    step (genFloat a b) s
