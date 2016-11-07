module Model exposing (..)

import Material
import GenericDict exposing (GenericDict)
import Extras


type alias Model =
    { mdl : Material.Model, board : Board, selected : Maybe Piece, rack : Rack, turnState : TurnState }


defaultState =
    { mdl = Material.model, board = emptyBoard, selected = Nothing, rack = fullRack, turnState = SelectPiece }


type TurnState
    = Win
    | Loss
    | SelectPiece
    | PlayPiece


type alias Rack =
    GenericDict Piece ()


emptyRack =
    GenericDict.empty pieceComparer


fullRack =
    piecePossibilities
        |> List.map (\piece -> ( piece, () ))
        |> GenericDict.fromList pieceComparer


isInRack : Piece -> Rack -> Bool
isInRack =
    GenericDict.member


type alias Board =
    { top : Floor
    , middle : Floor
    , bottom : Floor
    }


emptyBoard =
    Board emptyFloor emptyFloor emptyFloor


type alias Floor =
    { zeroZero : Space
    , zeroOne : Space
    , zeroTwo : Space
    , oneZero : Space
    , oneOne : Space
    , oneTwo : Space
    , twoZero : Space
    , twoOne : Space
    , twoTwo : Space
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


place : Piece -> FloorId -> SpaceId -> Board -> Board
place piece floorId spaceId board =
    let
        newFloor =
            setSpace spaceId (OccupiedSpace piece) (getFloor floorId board)
    in
        setFloor floorId newFloor board


setFloor : FloorId -> Floor -> Board -> Board
setFloor floorId newFloor board =
    case floorId of
        Top ->
            { board | top = newFloor }

        Middle ->
            { board | middle = newFloor }

        Bottom ->
            { board | bottom = newFloor }


getFloor floorId board =
    case floorId of
        Top ->
            board.top

        Middle ->
            board.middle

        Bottom ->
            board.bottom


setSpace spaceId space floor =
    case spaceId of
        ZeroZero ->
            { floor | zeroZero = space }

        OneZero ->
            { floor | oneZero = space }

        TwoZero ->
            { floor | twoZero = space }

        ZeroOne ->
            { floor | zeroOne = space }

        OneOne ->
            { floor | oneOne = space }

        TwoOne ->
            { floor | twoOne = space }

        ZeroTwo ->
            { floor | zeroTwo = space }

        OneTwo ->
            { floor | oneTwo = space }

        TwoTwo ->
            { floor | twoTwo = space }


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
