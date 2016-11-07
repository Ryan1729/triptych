module Msg exposing (..)

import Material
import Model exposing (..)


type Msg
    = Mdl (Material.Msg Msg)
    | NewGame
    | Place FloorId SpaceId
    | Select Piece
