module Rectangle exposing (..)

import Color exposing (..)
import Msg exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgAttr


type alias Rectangle =
    { color : Color
    , leftTopPosition : ( Float, Float )
    , width : Float
    , height : Float
    }


viewRectangle : Rectangle -> List (Svg Msg)
viewRectangle a =
    [ Svg.rect
        [ SvgAttr.width (String.fromFloat a.width ++ "px")
        , SvgAttr.height (String.fromFloat a.height ++ "px")
        , SvgAttr.x (String.fromFloat (Tuple.first a.leftTopPosition) ++ "px")
        , SvgAttr.y (String.fromFloat (Tuple.second a.leftTopPosition) ++ "px")
        , SvgAttr.fill (getcolor a.color)
        ]
        []
    ]
