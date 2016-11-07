module Extras exposing (..)

-- derived from http://stackoverflow.com/a/33625733/4496839
-- has less helper functions than the List.Extra version


indexOfhelper : List a -> a -> Int -> Maybe Int
indexOfhelper lst elem offset =
    case lst of
        [] ->
            Nothing

        x :: xs ->
            if x == elem then
                Just offset
            else
                indexOfhelper xs elem (offset + 1)


indexOf : List a -> a -> Maybe Int
indexOf lst element =
    indexOfhelper lst element 0



-- A lot of the time this default is fine


indexOfDefault : List a -> a -> Int
indexOfDefault lst element =
    indexOfhelper lst element 0
        |> Maybe.withDefault -1
