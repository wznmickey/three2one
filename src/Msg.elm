module Msg exposing (..)
type Demo
    = Demo1
    | Demo2


type alias Msg =
    { init : Demo
    }