port module Main exposing (..)

import Browser
import Html exposing (Html, div, text, time)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Loaders
import Random
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
      , theme = "Alone"
      , themes = ludumDareThemes
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
                    [ text "<click> to Pick a Theme..." ]
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


ludumDareThemes : List String
ludumDareThemes =
    -- https://en.wikipedia.org/wiki/Ludum_Dare#Results
    [ "Indirect interaction"
    , "Guardian"
    , "Construction/destruction" -- (sheep)
    , "Preparation – Set it up, let it go"
    , "Infection"
    , "Random"
    , "Light and darkness"
    , "Growth"
    , "Swarms"
    , "Moon/anti-text"
    , "Build the level you play"
    , "Chain reaction"
    , "Weird/unexpected/surprise"
    , "Minimalist"
    , "The tower" -- (owls)
    , "Roads"
    , "Advancing wall of doom"
    , "Caverns"
    , "Exploration"
    , "Islands"
    , "Enemies as weapons"
    , "Discovery"
    , "It's dangerous to go alone! Take this!"
    , "Escape"
    , "Alone" -- (kitten challenge)
    , "Tiny world"
    , "Evolution"
    , "You are the villain" -- (goat)
    , "Minimalism" -- (potato)
    , "10 seconds"
    , "You only get one"
    , "Beneath the surface"
    , "Connected Worlds"
    , "Entire Game on One Screen"
    , "An Unconventional Weapon"
    , "You are the Monster"
    , "Growing/two button controls"
    , "Shapeshift"
    , "Ancient Technology"
    , "One Room"
    , "A Small World"
    , "Running out of Power"
    , "The more you have, the worse it is"
    , "Combine two incompatible genres"
    , "Running out of space"
    , "Sacrifices must be made"
    , "Your life is currency"
    , "Start with nothing"
    , "Keep it alive"
    , "Stuck in a loop"
    , "Deeper and Deeper"
    , "Unstable"
    , "Delay the inevitable"
    , "Every 10 seconds"
    , "Harvest"
    , "Delivery"
    , "Limited space" -- 54
    ]
