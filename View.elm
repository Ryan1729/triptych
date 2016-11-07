module View exposing (view)

import Model exposing (Model, Board, Piece(..), Floor)
import Html exposing (Html, text)
import Msg exposing (Msg(..))
import Material.Button as Button
import Svg exposing (Svg, svg, polygon, Attribute, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


view : Model -> Html Msg
view model =
    Html.div []
        [ Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Button.ripple
            , Button.onClick NoOp
            ]
            [ text "test Button" ]
        , svg
            [ width boardWidthString
            , height boardHeightString
            , viewBox ("0 0 " ++ boardWidthString ++ " " ++ boardHeightString)
            ]
            [ renderBoard model.selected model.board
            ]
        ]


boardWidth =
    720


boardWidthString =
    toString boardWidth


boardHeight =
    720


boardHeightString =
    toString boardHeight


centerX =
    boardWidth / 2


centerY =
    boardHeight / 2



-- <linearGradient id="Gradient2" x1="0" x2="1" y1="0" y2="1">
--         <stop offset="0%" stop-color="red"></stop>
--         <stop offset="100%" stop-color="red" stop-opacity="0"></stop>
--
--       </linearGradient>


renderBoard : Maybe Piece -> Board -> Svg Msg
renderBoard selected board =
    g []
        <| renderFloor ( centerX, centerY - 4 * spaceHeight ) board.top
        ++ renderFloor ( centerX, centerY - 0.5 * spaceHeight ) board.middle
        ++ renderFloor ( centerX, centerY + 3 * spaceHeight ) board.bottom


renderFloor : ( Float, Float ) -> Floor -> List (Svg Msg)
renderFloor ( x, y ) floor =
    [ space ( x - scpaceWidth / 2, y - spaceHeight / 2 ) Nothing
    , space ( x + scpaceWidth / 2, y - spaceHeight / 2 ) Nothing
    , space ( x - scpaceWidth, y ) Nothing
    , space ( x, y - spaceHeight ) Nothing
    , space ( x, y ) Nothing
    , space ( x + scpaceWidth, y ) Nothing
    , space ( x, y + spaceHeight ) Nothing
    , space ( x - scpaceWidth / 2, y + spaceHeight / 2 ) Nothing
    , space ( x + scpaceWidth / 2, y + spaceHeight / 2 ) Nothing
    ]


space : ( Float, Float ) -> Maybe Msg -> Svg Msg
space ( x, y ) maybeMsg =
    let
        dString =
            ("M " ++ toString x ++ " " ++ toString y)
                ++ spaceSuffix

        msgAttributes =
            case maybeMsg of
                Just msg ->
                    [ stroke "white", onClick msg ]

                Nothing ->
                    [ stroke "black" ]
    in
        g []
            [ Svg.path
                (msgAttributes
                    ++ [ d dString
                       , fill "#444444"
                       , strokeWidth "4"
                       ]
                )
                []
            ]


spaceScale =
    80


spaceScaleString =
    toString spaceScale


minusSpaceScaleString =
    toString -spaceScale


halfSpaceScale =
    spaceScale / 2


halfSpaceScaleString =
    toString halfSpaceScale


minusHalfSpaceScaleString =
    toString -halfSpaceScale


scpaceWidth =
    spaceScale * 2


shortCornerHeight =
    spaceScale * 3 / 8


spaceHeight =
    shortCornerHeight * 2


shortCornerString =
    toString shortCornerHeight


minusShortCornerString =
    toString -shortCornerHeight


spaceSuffix =
    (" m 0 " ++ spaceScaleString)
        ++ (" l " ++ spaceScaleString ++ " " ++ minusShortCornerString)
        ++ (" l " ++ minusSpaceScaleString ++ " " ++ minusShortCornerString)
        ++ (" l " ++ minusSpaceScaleString ++ " " ++ shortCornerString)
        ++ "Z"
