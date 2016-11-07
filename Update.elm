module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model)
import Material


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( Model.defaultState, Cmd.none )

        Place floorId spaceId ->
            case model.selected of
                Just piece ->
                    ( { model | board = Model.place piece floorId spaceId model.board }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Select piece ->
            ( { model | selected = Just piece }, Cmd.none )

        Mdl msg' ->
            Material.update msg' model
