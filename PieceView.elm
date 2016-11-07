module PieceView exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, svg, rect, polygon, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Msg exposing (Msg(..))
import Model exposing (Piece(..), Rack, Shape(..), Colour(..), Pattern(..))


rackWidth =
    250


rackHeight =
    600


rackWidthString =
    toString rackWidth


rackHeightString =
    toString rackHeight


renderRack : Maybe Piece -> Rack -> Html Msg
renderRack selected rack =
    svg
        [ width rackWidthString
        , height rackHeightString
        , viewBox ("0 0 " ++ rackWidthString ++ " " ++ rackHeightString)
        ]
        <| [ Svg.defs []
                [ Svg.linearGradient [ id "RedGradient", x1 "0", x2 "1", y1 "0", y2 "1" ]
                    [ Svg.stop [ offset "12.5%", stopColor (colourToString Red) ] []
                    , Svg.stop [ offset "87.5%", stopColor (colourToString Red), stopOpacity "0" ] []
                    ]
                , Svg.linearGradient [ id "GreenGradient", x1 "0", x2 "1", y1 "0", y2 "1" ]
                    [ Svg.stop [ offset "12.5%", stopColor (colourToString Green) ] []
                    , Svg.stop [ offset "87.5%", stopColor (colourToString Green), stopOpacity "0" ] []
                    ]
                , Svg.linearGradient [ id "BlueGradient", x1 "0", x2 "1", y1 "0", y2 "1" ]
                    [ Svg.stop [ offset "12.5%", stopColor (colourToString Blue) ] []
                    , Svg.stop [ offset "87.5%", stopColor (colourToString Blue), stopOpacity "0" ] []
                    ]
                ]
             -- <linearGradient id="Gradient2" x1="0" x2="1" y1="0" y2="1">
             --         <stop offset="0%" stop-color="red"></stop>
             --         <stop offset="100%" stop-color="red" stop-opacity="0"></stop>
             --
             -- </linearGradient>
           , Svg.rect
                [ x "0"
                , y "0"
                , width rackWidthString
                , height rackHeightString
                , stroke "black"
                , strokeWidth "2"
                , fillOpacity "0"
                ]
                []
           ]
        ++ renderPieces selected rack


renderPieces : Maybe Piece -> Rack -> List (Svg Msg)
renderPieces selected rack =
    let
        isSelected =
            case selected of
                Just selectedPiece ->
                    (==) selectedPiece

                Nothing ->
                    always False
    in
        Model.piecePossibilities
            |> List.indexedMap
                (\index piece ->
                    renderPieceInRack (indexToPosition index)
                        (isSelected piece)
                        (Model.isInRack piece rack)
                        piece
                )


nullSvg =
    Svg.text ""


renderPieceInRack : ( Float, Float ) -> Bool -> Bool -> Piece -> Svg Msg
renderPieceInRack (( xPos, yPos ) as point) isSelected isPresent piece =
    if isPresent then
        let
            renderedPiece =
                renderPiece [ onClick (Select piece) ] point piece
        in
            if isSelected then
                g []
                    [ renderedPiece
                    , selectedIndicator point
                    ]
            else
                renderedPiece
    else
        nullSvg


renderPiece : List (Svg.Attribute Msg) -> ( Float, Float ) -> Piece -> Svg Msg
renderPiece extraAttributes ( xPos, yPos ) (Piece shape colour pattern) =
    let
        pieceAtrributes =
            case pattern of
                Full ->
                    [ fill (colourToString colour) ]

                StrokeOnly ->
                    [ fillOpacity "0.0"
                    , stroke (colourToString colour)
                    , strokeWidth "4"
                    ]

                Gradient ->
                    [ fill (colourToGradientString colour) ]
    in
        rect
            (extraAttributes
                ++ pieceAtrributes
                ++ [ x (toString xPos)
                   , y (toString yPos)
                   , width (toString pieceWidth)
                   , height (toString pieceHeight)
                   ]
            )
            []


colourToString colour =
    case colour of
        Red ->
            "#FF4136"

        Green ->
            "#2ECC40"

        Blue ->
            "#0074D9"


colourToGradientString colour =
    case colour of
        Red ->
            "url(#RedGradient)"

        Green ->
            "url(#GreenGradient)"

        Blue ->
            "url(#BlueGradient)"


selectedIndicator ( xPos, yPos ) =
    rect
        [ fillOpacity "0.0"
        , x (toString (xPos - 1.5))
        , y (toString (yPos - 1.5))
        , width (toString (pieceWidth + 3))
        , height (toString (pieceHeight + 3))
        , stroke "#FFDC00"
        , strokeWidth "2"
        ]
        []


left : Float
left =
    pieceWidth - spacing


center : Float
center =
    2 * pieceWidth


right : Float
right =
    3 * pieceWidth + spacing


spacing : Float
spacing =
    15


pieceWidth : Float
pieceWidth =
    50


pieceHeight : Float
pieceHeight =
    50


indexToPosition : Int -> ( Float, Float )
indexToPosition index =
    let
        x =
            case index % 3 of
                0 ->
                    left

                1 ->
                    center

                _ ->
                    right

        y =
            toFloat (index // 3) * (pieceHeight + spacing) + spacing
    in
        ( x, y )