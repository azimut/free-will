port module Main exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Time
import Browser
import Html.Events exposing (onClick)
import Html exposing (Html, text, div, time)
import Html.Attributes exposing (class)
import Random

main : Program (List (Maybe String)) Model Msg
main =
  Browser.element
    { init = init, update = update, view = view, subscriptions = subscriptions }

type alias Model =
    { choice  : Maybe String
    , theme   : String
    , themes  : List String
    , now     : Time.Posix
    , start   : Time.Posix
    , zone    : Time.Zone
    }

deFormatTime : String -> Time.Posix
deFormatTime stime =
    case (stime |> String.split ":" |> List.map String.toInt |> List.map (Maybe.withDefault 0)) of
        hour :: minute :: second :: [] ->
            Time.millisToPosix (second * 1000 + minute * 60 * 1000 + hour * 60 * 60 * 1000)
        _                              ->
            Time.millisToPosix 0

decodeFlags : List (Maybe String) -> (Maybe String, Time.Posix)
decodeFlags flags =
    case flags of
        Just choice :: Just start :: []  ->
            case (Decode.decodeString Decode.string choice, Decode.decodeString Decode.string start) of
                (Ok c, Ok s) -> (Just c, deFormatTime s)
                _            -> (Nothing, Time.millisToPosix 0)
        _ -> (Nothing, Time.millisToPosix 0)

init : List (Maybe String) -> (Model, Cmd msg)
init flags =
    let
        (storedChoice, storedStart) = decodeFlags flags
    in
        ({ choice = storedChoice
         , theme  = "Alone"
         , themes = ludumDareThemes
         , now    = Time.millisToPosix 0
         , start  = storedStart
         , zone   = Time.utc }
        , Cmd.none)

formatDigit : Int -> String
formatDigit = String.fromInt >> String.padLeft 2 '0'

formatTime : Time.Zone -> Time.Posix -> String
formatTime zone time =
    [ Time.toHour zone time
    , Time.toMinute zone time
    , Time.toSecond zone time
    ] |> List.map formatDigit |> String.join ":"

countDown : Time.Posix -> Time.Posix -> Time.Posix
countDown now start =
    let
        aDayMillis = (24 * 60 * 60 * 1000) - 1 -- offset 1 to avoid 00:00:00
        dtMillis = Time.posixToMillis now - Time.posixToMillis start
    in
        Time.millisToPosix (aDayMillis - dtMillis)

view : Model -> Html Msg
view { choice, start, now, zone } =
    case choice of
        Nothing -> div [onClick PickNew, class "container"]
                   [ div [class "theme"]
                         [text "<click> to Pick a Theme..."] ]
        Just s  -> div [onClick PickNew, class "container"]
                   [ div [class "theme"]
                         [text s]
                   , time [] [text (countDown now start |>formatTime zone)] ]

type Msg
    = PickNew
    | NewChoice String
    | Tick Time.Posix

port storeChoice : String -> Cmd msg

saveChoice : String -> Cmd msg
saveChoice choice =
    choice |>Encode.string
           |>Encode.encode 0
           |>storeChoice

port storeStart : String -> Cmd msg

saveStart : Time.Zone -> Time.Posix -> Cmd msg
saveStart zone time =
    formatTime zone time
        |>Encode.string
        |>Encode.encode 0
        |>storeStart

save : Time.Zone -> Time.Posix -> String -> Cmd msg
save zone time choice = Cmd.batch [saveStart zone time, saveChoice choice]

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ themes, theme, now, zone } as model) =
    case msg of
        Tick t        -> ({ model | now = t }, Cmd.none)
        NewChoice new -> ({ model | choice = Just new, start = now }, save zone now new )
        PickNew       -> (model, (Random.generate NewChoice (Random.uniform theme themes)))

subscriptions : model -> Sub Msg
subscriptions _ = Time.every 1000 Tick

ludumDareThemes : List String
ludumDareThemes =
    ["Indirect interaction"
    ,"Guardian"
    ,"Construction/destruction (sheep)"
    ,"Preparation – Set it up, let it go"
    ,"Infection"
    ,"Random"
    ,"Light and darkness"
    ,"Growth"
    ,"Swarms"
    ,"Moon/anti-text"
    ,"Build the level you play"
    ,"Chain reaction"
    ,"Weird/unexpected/surprise"
    ,"Minimalist"
    ,"The tower (owls)"
    ,"Roads"
    ,"Advancing wall of doom"
    ,"Caverns"
    ,"Exploration"
    ,"Islands"
    ,"Enemies as weapons"
    ,"Discovery"
    ,"It's dangerous to go alone! Take this!"
    ,"Escape"
    ,"Alone (kitten challenge)"
    ,"Tiny world"
    ,"Evolution"
    ,"You are the villain (goat)"
    ,"Minimalism (potato)"
    ,"10 seconds"
    ,"You only get one"
    ,"Beneath the surface"
    ,"Connected Worlds"
    ,"Entire Game on One Screen"
    ,"An Unconventional Weapon"
    ,"You are the Monster"
    ,"Growing/two button controls"
    ,"Shapeshift"
    ,"Ancient Technology"
    ,"One Room"
    ,"A Small World"
    ,"Running out of Power"
    ,"The more you have, the worse it is"
    ,"Combine two incompatible genres"
    ,"Running out of space"
    ,"Sacrifices must be made"
    ,"Your life is currency"
    ,"Start with nothing"
    ,"Keep it alive"
    ,"Stuck in a loop"
    ,"Deeper and Deeper"
    ,"Unstable"
    ,"Delay the inevitable"
    ,"Every 10 seconds"
    ,"Harvest"
    ,"Delivery"
    ,"Limited space" -- 54
    ]
