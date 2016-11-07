module Model exposing (..)

import Material
import GenericDict exposing (GenericDict)
import Extras


type alias Model =
    { mdl : Material.Model, board : Board, selected : Maybe Piece, rack : Rack, outcome : Outcome }


defaultState =
    { mdl = Material.model, board = emptyBoard, selected = Nothing, rack = fullRack, outcome = TBD }


type Outcome
    = TBD
    | Win
    | Loss


type alias Rack =
    GenericDict Piece ()


emptyRack =
    GenericDict.empty pieceComparer


fullRack =
    piecePossibilities
        |> List.map (\piece -> ( piece, () ))
        |> GenericDict.fromList pieceComparer


type alias Board =
    { top : Floor
    , middle : Floor
    , bottom : Floor
    }


emptyBoard =
    Board emptyFloor emptyFloor emptyFloor


type alias Floor =
    { oneOne : Space
    , oneTwo : Space
    , oneZero : Space
    , twoOne : Space
    , twoTwo : Space
    , twoZero : Space
    , zeroOne : Space
    , zeroTwo : Space
    , zeroZero : Space
    }


emptyFloor =
    Floor EmptySpace
        EmptySpace
        EmptySpace
        EmptySpace
        EmptySpace
        EmptySpace
        EmptySpace
        EmptySpace
        EmptySpace


type FloorId
    = Top
    | Middle
    | Bottom


type Space
    = EmptySpace
    | OccupiedSpace Piece


type SpaceId
    = ZeroZero
    | OneZero
    | TwoZero
    | ZeroOne
    | OneOne
    | TwoOne
    | ZeroTwo
    | OneTwo
    | TwoTwo


type Piece
    = Piece Shape Colour Pattern


pieceComparer : Piece -> Piece -> Order
pieceComparer (Piece s1 c1 p1) (Piece s2 c2 p2) =
    case
        compare (Extras.indexOfDefault shapePossibilities s1)
            (Extras.indexOfDefault shapePossibilities s2)
    of
        EQ ->
            case
                compare (Extras.indexOfDefault colourPossibilities c1)
                    (Extras.indexOfDefault colourPossibilities c2)
            of
                EQ ->
                    compare (Extras.indexOfDefault patternPossibilities p1)
                        (Extras.indexOfDefault patternPossibilities p2)

                determeined ->
                    determeined

        determeined ->
            determeined


type Shape
    = Circle
    | Triangle
    | Square


shapePossibilities =
    [ Circle
    , Triangle
    , Square
    ]


type Colour
    = Red
    | Green
    | Blue


colourPossibilities =
    [ Red
    , Green
    , Blue
    ]


type Pattern
    = Full
    | StrokeOnly
    | Gradient


patternPossibilities =
    [ Full
    , StrokeOnly
    , Gradient
    ]


piecePossibilities =
    List.concatMap
        (\shape ->
            List.concatMap
                (\colour ->
                    List.map (Piece shape colour) patternPossibilities
                )
                colourPossibilities
        )
        shapePossibilities
