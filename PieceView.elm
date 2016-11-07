module PieceView exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, svg, polygon, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Msg exposing (Msg(..))
import Model exposing (Piece, Rack)


stashWidth =
    500


stashHeight =
    600


stashWidthString =
    toString stashWidth


stashHeightString =
    toString stashHeight


renderRack : Maybe Piece -> Rack -> Html Msg
renderRack selected rack =
    svg
        [ width stashWidthString
        , height stashHeightString
        , viewBox ("0 0 " ++ stashWidthString ++ " " ++ stashHeightString)
        ]
        <| [ Svg.rect
                [ x "0"
                , y "0"
                , width stashWidthString
                , height stashHeightString
                , stroke "black"
                , strokeWidth "2"
                , fillOpacity "0"
                ]
                []
           ]
