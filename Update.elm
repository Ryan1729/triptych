module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model, TurnState(..))
import Material


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( Model.defaultState, Cmd.none )

        Place floorId spaceId ->
            case model.selected of
                Just piece ->
                    let
                        newModel =
                            { model
                                | board = Model.place piece floorId spaceId model.board
                                , rack = Model.removeFromRack piece model.rack
                                , selected = Nothing
                            }
                    in
                        if checkForAnyLines newModel then
                            ( { newModel | turnState = Win }, Cmd.none )
                        else
                            ( newModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Select piece ->
            ( cpuTurn { model | selected = Just piece }, Cmd.none )

        Mdl msg' ->
            Material.update msg' model


cpuTurn : Model -> Model
cpuTurn model =
    let
        newModel =
            makeCpuMove model
    in
        if checkForAnyLines newModel then
            { newModel | turnState = Loss }
        else
            newModel


makeCpuMove model =
    model


checkForAnyLines : Model -> Bool
checkForAnyLines model =
    False



-- andCheckLine : Stack -> Stack -> Stack -> SubOutcome -> SubOutcome
-- andCheckLine stack1 stack2 stack3 outcome =
--     case outcome of
--         Undetermined ->
--             checkLine stack1 stack2 stack3
--
--         predetermined ->
--             predetermined
