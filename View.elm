module View exposing (view)

import Model exposing (Model, Board, Piece(..), Floor, FloorId(..), Space(..), SpaceId(..))
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
            , Button.onClick NewGame
            ]
            [ text "New Game" ]
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
        <| renderFloor ( centerX, centerY - 4 * spaceHeight ) Top board.top
        ++ renderFloor ( centerX, centerY - 0.5 * spaceHeight ) Middle board.middle
        ++ renderFloor ( centerX, centerY + 3 * spaceHeight ) Bottom board.bottom


renderFloor : ( Float, Float ) -> FloorId -> Floor -> List (Svg Msg)
renderFloor ( x, y ) floorId floor =
    [ space ( x, y - spaceHeight ) floorId ZeroZero floor.zeroZero
    , space ( x + scpaceWidth / 2, y - spaceHeight / 2 ) floorId OneZero floor.oneZero
    , space ( x + scpaceWidth, y ) floorId TwoZero floor.twoZero
    , space ( x - scpaceWidth / 2, y - spaceHeight / 2 ) floorId ZeroOne floor.zeroOne
    , space ( x, y ) floorId OneOne floor.oneOne
    , space ( x + scpaceWidth / 2, y + spaceHeight / 2 ) floorId TwoOne floor.twoOne
    , space ( x - scpaceWidth, y ) floorId ZeroTwo floor.zeroTwo
    , space ( x - scpaceWidth / 2, y + spaceHeight / 2 ) floorId OneTwo floor.oneTwo
    , space ( x, y + spaceHeight ) floorId TwoTwo floor.twoTwo
    ]


space : ( Float, Float ) -> FloorId -> SpaceId -> Space -> Svg Msg
space ( x, y ) floorId spaceId space =
    let
        dString =
            ("M " ++ toString x ++ " " ++ toString y)
                ++ spaceSuffix

        ( msgAttributes, pieceSvg ) =
            case space of
                EmptySpace ->
                    ( [ stroke "white"
                      , onClick
                            (Place floorId
                                spaceId
                            )
                      ]
                    , nullSvg
                    )

                OccupiedSpace piece ->
                    ( [ stroke "black" ], renderPiece x y piece )
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


renderPiece : Float -> Float -> Piece -> Svg Msg
renderPiece x y piece =
    nullSvg


nullSvg =
    Svg.polygon [] []


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
