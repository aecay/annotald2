module TreeEdit.Index exposing
    ( Index
    , Variety(..)
    , gap
    , normal
    , number
    , string
    , variety
    )

import String exposing (fromInt)
import Monocle.Lens as Lens exposing (Lens)


type Variety
    = Normal
    | Gap


type alias Index =
    { number : Int
    , variety : Variety
    }


normal : Int -> Index
normal i =
    { number = i, variety = Normal }


gap : Int -> Index
gap i =
    { number = i, variety = Gap }


number : Lens Index Int
number =
    Lens .number (\i x -> { x | number = i })


variety : Lens Index Variety
variety =
    Lens .variety (\v x -> { x | variety = v })


string : Index -> String
string index =
    let
        vstr =
            case index.variety of
                Normal ->
                    "-"

                Gap ->
                    "="
    in
    vstr ++ fromInt index.number
