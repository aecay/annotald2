module Index exposing (Index
                      , Variety(..)
                      , normal
                      , gap
                      , string
                      , number
                      , variety)

import Monocle.Lens as Lens exposing (Lens)

type Variety = Normal | Gap

type alias Index = { number : Int
                   , variety: Variety
                   }

normal : Int -> Index
normal i = { number = i, variety = Normal }

gap : Int -> Index
gap i = { number = i, variety = Gap }

number : Lens Index Int
number = Lens .number (\i x -> { x | number = i })

variety : Lens Index Variety
variety = Lens .variety (\v x -> { x | variety = v })

string : Index -> String
string { number, variety } =
    let
        vstr = case variety of
                   Normal -> "-"
                   Gap -> "="
    in vstr ++ toString number
