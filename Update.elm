module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (..)
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
    checkMultiFloorLines model.board
        |> andCheckFloor Top model.board
        |> andCheckFloor Middle model.board
        |> andCheckFloor Bottom model.board


checkMultiFloorLines : Board -> Bool
checkMultiFloorLines board =
    let
        topFloor =
            Model.getFloor Top board

        middleFloor =
            Model.getFloor Middle board

        bottomFloor =
            Model.getFloor Bottom board
    in
        (--vertical lines
         checkThreeByThree topFloor.zeroZero
            topFloor.oneZero
            topFloor.twoZero
            middleFloor.zeroZero
            middleFloor.oneZero
            middleFloor.twoZero
            bottomFloor.zeroZero
            bottomFloor.oneZero
            bottomFloor.twoZero
            |> andCheckThreeByThree topFloor.zeroOne
                topFloor.oneOne
                topFloor.twoOne
                middleFloor.zeroOne
                middleFloor.oneOne
                middleFloor.twoOne
                bottomFloor.zeroOne
                bottomFloor.oneOne
                bottomFloor.twoOne
            |> andCheckThreeByThree topFloor.zeroTwo
                topFloor.oneTwo
                topFloor.twoTwo
                middleFloor.zeroTwo
                middleFloor.oneTwo
                middleFloor.twoTwo
                bottomFloor.zeroTwo
                bottomFloor.oneTwo
                bottomFloor.twoTwo
        )
            |> -- diagonals
               andCheckThreeByThree topFloor.zeroZero
                topFloor.oneZero
                topFloor.twoZero
                middleFloor.zeroOne
                middleFloor.oneOne
                middleFloor.twoOne
                bottomFloor.zeroTwo
                bottomFloor.oneTwo
                bottomFloor.twoTwo
            |> andCheckThreeByThree topFloor.zeroZero
                topFloor.zeroOne
                topFloor.zeroTwo
                middleFloor.oneZero
                middleFloor.oneOne
                middleFloor.oneTwo
                bottomFloor.twoZero
                bottomFloor.twoOne
                bottomFloor.twoTwo
            |> andCheckThreeByThree topFloor.zeroTwo
                topFloor.oneTwo
                topFloor.twoTwo
                middleFloor.zeroOne
                middleFloor.oneOne
                middleFloor.twoOne
                bottomFloor.zeroZero
                bottomFloor.oneZero
                bottomFloor.twoZero
            |> andCheckThreeByThree topFloor.twoZero
                topFloor.twoOne
                topFloor.twoTwo
                middleFloor.oneZero
                middleFloor.oneOne
                middleFloor.oneTwo
                bottomFloor.zeroZero
                bottomFloor.zeroOne
                bottomFloor.zeroTwo


andCheckFloor : FloorId -> Board -> Bool -> Bool
andCheckFloor floorId board previousResult =
    if previousResult then
        previousResult
    else
        checkFloor floorId board


checkFloor : FloorId -> Board -> Bool
checkFloor floorId board =
    let
        floor =
            Model.getFloor floorId board
    in
        checkThreeByThree floor.zeroZero
            floor.oneZero
            floor.twoZero
            floor.zeroOne
            floor.oneOne
            floor.twoOne
            floor.zeroTwo
            floor.oneTwo
            floor.twoTwo


checkThreeByThree space1 space2 space3 space4 space5 space6 space7 space8 space9 =
    checkLine space1 space2 space3
        |> andCheckLine space4 space5 space6
        |> andCheckLine space7 space8 space9
        |> andCheckLine space1 space4 space7
        |> andCheckLine space2 space5 space8
        |> andCheckLine space3 space6 space9
        |> andCheckLine space1 space5 space9
        |> andCheckLine space3 space5 space7


andCheckThreeByThree space1 space2 space3 space4 space5 space6 space7 space8 space9 previousResult =
    if previousResult then
        previousResult
    else
        checkThreeByThree space1 space2 space3 space4 space5 space6 space7 space8 space9


checkLine : Space -> Space -> Space -> Bool
checkLine space1 space2 space3 =
    case ( space1, space2, space3 ) of
        ( OccupiedSpace piece1, OccupiedSpace piece2, OccupiedSpace piece3 ) ->
            checkPiecesMatch piece1 piece2 piece3

        _ ->
            False


andCheckLine : Space -> Space -> Space -> Bool -> Bool
andCheckLine space1 space2 space3 previousResult =
    if previousResult then
        previousResult
    else
        checkLine space1 space2 space3
