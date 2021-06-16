module RandomNum exposing (..)

import Msg exposing (..)
import Random exposing (..)


genInt : Int -> Int -> Generator Int
genInt a b =
    Random.int a b


genFloat : Float -> Float -> Generator Float
genFloat a b =
    Random.float a b


getRandomInt : Int -> Int -> Seed -> ( Int, Seed )
getRandomInt a b s =
    step (genInt a b) s


getRandomFloat : Float -> Float -> Seed -> ( Float, Seed )
getRandomFloat a b s =
    step (genFloat a b) s
