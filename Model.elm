module Model exposing (..)

import Material


type alias Model =
    { mdl : Material.Model, board : Board, selected : Maybe Piece }


defaultState =
    { mdl = Material.model, board = EmptyBoard, selected = Nothing }


type Board
    = EmptyBoard


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
