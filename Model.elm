module Model exposing (..)

import Material


type alias Model =
    { mdl : Material.Model, board : Board, selected : Maybe Piece }


defaultState =
    { mdl = Material.model, board = emptyBoard, selected = Nothing }


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


type Shape
    = Circle
    | Triangle
    | Square


type Colour
    = Red
    | Green
    | Blue


type Pattern
    = Full
    | StrokeOnly
    | Gradient
