module Main exposing (..)

import Time
import Browser
import Html.Events exposing (onClick)
import Html exposing (Html, text, div, time)
import Html.Attributes exposing (class)
import Random

main : Program () Model Msg
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

init : flags -> (Model, Cmd msg)
init _ = ({ choice = Nothing
          , theme  = "Alone"
          , themes = ludumDareThemes
          , now    = Time.millisToPosix 0
          , start  = Time.millisToPosix 0
          , zone   = Time.utc }
         , Cmd.none)

view : Model -> Html Msg
view { choice, start, now, zone } =
    case choice of
        Nothing -> div [onClick PickNew, class "container"]
                   [ div [class "theme"]
                         [text "<click> to Pick a Theme..."] ]
        Just s  -> div [onClick PickNew, class "container"]
                   [ div [class "theme"]
                         [text s]
                   , time [] [text (String.fromInt ((((Time.posixToMillis now) - (Time.posixToMillis start))//1000)  ))] ]

type Msg
    = PickNew
    | NewChoice String
    | Tick Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ themes, theme, now } as model) =
    case msg of
        Tick t        -> ({ model | now = t}, Cmd.none)
        NewChoice new -> ({ model | choice = Just new, start = now }, Cmd.none)
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
