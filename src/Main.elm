module Main exposing (..)

import Browser
import Css exposing (..)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (list)
import Package exposing (..)
import Set exposing (Set)
import Task



---- MODEL ----


type alias Model =
    { packages : List Package, showDetails : Set String }


init : ( Model, Cmd Msg )
init =
    ( { packages = [], showDetails = Set.fromList [] }
    , Cmd.none
      -- Http.get
      -- { url = "deps.json"
      -- , expect = Http.expectJson GotPackages (list packageDecoder)
      -- }
    )



---- UPDATE ----


type Msg
    = NoOp
    | FileRequested
    | FileSelected File
    | FileLoaded String
    | ShowDetails String
    | HideDetails String



-- loadFile : File -> Cmd Msg
-- loadFile f =


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowDetails p ->
            ( { model | showDetails = Set.insert p model.showDetails }, Cmd.none )

        HideDetails p ->
            ( { model | showDetails = Set.filter ((/=) p) model.showDetails }, Cmd.none )

        FileRequested ->
            ( model, Select.file [ "application/json" ] FileSelected )

        FileSelected f ->
            ( model, Task.perform FileLoaded (File.toString f) )

        FileLoaded s ->
            case Decode.decodeString (list packageDecoder) s of
                Ok packages ->
                    ( { model | packages = packages }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


showPackage : Model -> Package -> Html Msg
showPackage model pkg =
    let
        details =
            if Set.member pkg.name model.showDetails then
                [ ul [] (List.map (\n -> li [] [ text n ]) (dependants model.packages pkg.name)) ]

            else
                []

        detailsButton =
            if Set.member pkg.name model.showDetails then
                button [ onClick (HideDetails pkg.name) ] [ text "hide details" ]

            else
                button [ onClick (ShowDetails pkg.name) ] [ text "show details" ]
    in
    li []
        (p []
            [ text pkg.name
            , detailsButton
            ]
            :: details
        )


view : Model -> Html Msg
view model =
    case model.packages of
        [] ->
            div [] [ button [ onClick FileRequested ] [ text "Load Deps JSON" ] ]

        packages ->
            div []
                [ h1 [] [ text "Packages" ]
                , ul [] (List.map (showPackage model) (List.sortBy (.name >> String.toLower) packages))
                ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
