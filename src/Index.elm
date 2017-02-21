module Index exposing (Index
                      , Variety(..)
                      , normal
                      , gap
                      , string
                      , number)

type Variety = Normal | Gap

type alias Index = { number : Int
                   , variety: Variety
                   }

normal : Int -> Index
normal i = { number = i, variety = Normal }

gap : Int -> Index
gap i = { number = i, variety = Gap }

number : Index -> Int
number = .number

string : Index -> String
string { number, variety } =
    let
        vstr = case variety of
                   Normal -> "-"
                   Gap -> "="
    in vstr ++ toString number
