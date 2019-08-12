module Package exposing (..)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import Set exposing (Set)


type alias Package =
    { name : String, dependencies : List String }


packageDecoder : Decoder Package
packageDecoder =
    Decode.succeed Package
        |> required "name" string
        |> required "dependencies" (list string)


dependencies : List Package -> String -> List String
dependencies allPackages pkgName =
    let
        pkgs =
            List.filter (\a -> a.name == pkgName) allPackages
    in
    case pkgs of
        pkg :: [] ->
            List.concatMap (dependencies allPackages) pkg.dependencies
                |> (::) pkg.name

        _ ->
            []


dependants : List Package -> String -> List String
dependants allPackages pkgName =
    let
        pkgs =
            List.filter (\p -> List.member pkgName p.dependencies) allPackages
                |> List.map .name
    in
    pkgs
        ++ List.concatMap (dependants allPackages) pkgs
        |> Set.fromList
        |> Set.toList
