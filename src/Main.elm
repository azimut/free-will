port module Main exposing (..)

import Browser
import Html exposing (Html, div, text, time)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Loaders
import Random
import Themes
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init, update = update, view = view, subscriptions = subscriptions }


type Status
    = Selected String Time.Posix Time.Posix -- choice start now
    | Restored String Time.Posix -- choice start
    | Waiting Time.Posix -- now
    | New


type alias Model =
    { status : Status
    , theme : String
    , themes : List String
    , zone : Time.Zone
    }


type alias Flags =
    { savedChoice : Maybe String
    , savedStart : Maybe String
    }


decodeFlags : Flags -> Status
decodeFlags { savedChoice, savedStart } =
    case ( savedChoice, savedStart ) of
        ( Just choice, Just start ) ->
            case ( Decode.decodeString Decode.string choice, Decode.decodeString Decode.int start ) of
                ( Ok c, Ok s ) ->
                    Restored c (Time.millisToPosix s)

                _ ->
                    New

        _ ->
            New


init : Flags -> ( Model, Cmd msg )
init flags =
    ( { status = decodeFlags flags
      , theme = "undefined"
      , themes = Themes.all
      , zone = Time.utc
      }
    , Cmd.none
    )


formatDigit : Int -> String
formatDigit =
    String.fromInt >> String.padLeft 2 '0'


formatTime : Time.Zone -> Time.Posix -> String
formatTime zone time =
    [ Time.toHour zone time
    , Time.toMinute zone time
    , Time.toSecond zone time
    ]
        |> List.map formatDigit
        |> String.join ":"


countDown : Time.Posix -> Time.Posix -> Time.Posix
countDown now start =
    let
        -- offset -1 to avoid 00:00:00
        aDayMillis =
            (24 * 60 * 60 * 1000) - 1

        dtMillis =
            Time.posixToMillis now - Time.posixToMillis start
    in
    Time.millisToPosix (aDayMillis - dtMillis)


view : Model -> Html Msg
view { status, zone } =
    case status of
        Selected choice start now ->
            div [ class "container" ]
                [ div [ class "theme" ]
                    [ text choice ]
                , time [ class "timeout" ] [ text (countDown now start |> formatTime zone) ]
                ]

        Waiting _ ->
            div [ onClick PickNew, class "container" ]
                [ div [ class "theme" ]
                    [ text "<click me>" ]
                ]

        _ ->
            div [ class "container" ]
                [ div [ class "theme" ]
                    [ Loaders.puff 50 "#000" ]
                ]


type Msg
    = PickNew
    | NewChoice String
    | Tick Time.Posix


port storeChoice : String -> Cmd msg


saveChoice : String -> Cmd msg
saveChoice choice =
    choice
        |> Encode.string
        |> Encode.encode 0
        |> storeChoice


port storeStart : String -> Cmd msg


saveStart : Time.Posix -> Cmd msg
saveStart time =
    Time.posixToMillis time
        |> String.fromInt
        |> storeStart


save : Time.Posix -> String -> Cmd msg
save time choice =
    Cmd.batch [ saveStart time, saveChoice choice ]


hasExpired : Time.Posix -> Time.Posix -> Bool
hasExpired start now =
    Time.posixToMillis now - Time.posixToMillis start > 24 * 60 * 60 * 1000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ themes, theme, status } as model) =
    case status of
        Waiting prev ->
            case msg of
                NewChoice new ->
                    ( { model | status = Selected new prev prev }, save prev new )

                Tick now ->
                    ( { model | status = Waiting now }, Cmd.none )

                PickNew ->
                    ( model, Random.generate NewChoice (Random.uniform theme themes) )

        New ->
            case msg of
                Tick now ->
                    ( { model | status = Waiting now }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Restored choice start ->
            case msg of
                Tick now ->
                    ( { model
                        | status =
                            if hasExpired start now then
                                Waiting now

                            else
                                Selected choice start now
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Selected choice start prev ->
            case msg of
                Tick now ->
                    ( { model
                        | status =
                            if hasExpired start now then
                                Waiting now

                            else
                                Selected choice start now
                      }
                    , Cmd.none
                    )

                NewChoice new ->
                    ( { model | status = Selected new prev prev }, save prev new )

                PickNew ->
                    ( model, Random.generate NewChoice (Random.uniform theme themes) )


subscriptions : model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick
