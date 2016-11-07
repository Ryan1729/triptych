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
    Board EmptyFloor EmptyFloor EmptyFloor


type Floor
    = EmptyFloor


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
