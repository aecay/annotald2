module TreeEdit.Path exposing ( Path(..)
                              , PathFragment
                              , singleton
                              , childPath
                              , root
                              , parent
                              , splitCommon
                              , join
                              , join3
                              , isFragEmpty
                              , shiftOne
                              , foot
                              , allCombos
                              , toFragment
                              , internals
                              , advance
                              )

import List.Extra

import TreeEdit.Utils as Utils exposing (fromJust)

type Path = Path Int (List Int) | RootPath

type PathFragment = PF (List Int)

internals = { fromList = fromList
            , pf = PF
            }

childPath : Int -> Path -> Path
childPath idx path =
    case path of
        RootPath -> Path idx []
        Path foot leg -> Path idx (foot :: leg)

singleton : Int -> Path
singleton idx =
    Path idx []

root : Path -> Path
root path =
    case path of
        RootPath -> RootPath
        Path foot leg ->
            case leg of
                [] -> singleton foot
                otherwise -> root (parent path)

parent : Path -> Path
parent path =
    case path of
        RootPath -> Debug.crash "Can't get the parent of the root!"
        Path _ [] -> RootPath
        Path _ (l :: ls) -> Path l ls

toList : Path -> List Int
toList path =
    case path of
        RootPath -> []
        Path foot leg -> foot :: leg

fromList : List Int -> Path
fromList l =
    case l of
        [] -> RootPath
        l :: ls -> Path l ls

toFragment : Path -> PathFragment
toFragment = PF << toList

foot : Path -> Int
foot p =
    case p of
        RootPath -> Debug.crash "Root has no foot"
        Path foot _ -> foot

join : Path -> PathFragment -> Path
join path (PF frag) =
    case path of
        RootPath -> fromList frag
        Path pfoot pleg -> case frag of
                         [] -> path
                         ffoot :: fleg -> Path ffoot (fleg ++ (pfoot :: pleg))

join3 : Path -> Int -> PathFragment -> Path
join3 path step (PF frag) =
    case path of
        RootPath -> join (singleton step) (PF frag)
        Path foot leg -> case frag of
                         [] -> Path step (foot :: leg)
                         ffoot :: fleg -> Path ffoot (fleg ++ (step :: foot :: leg))


type alias SplitResult = { common : Path
                         , sibFrom : Maybe Int
                         , sibTo : Maybe Int
                         , tailFrom : PathFragment
                         , tailTo : PathFragment
                         , fragFrom : PathFragment
                         , fragTo : PathFragment
                         }
splitCommon : Path -> Path -> SplitResult
splitCommon p1 p2 =
    let
        go p1 p2 accum = case (List.head p1, List.head p2) of
                             (Just h1, Just h2) -> if h1 == h2
                                                   then let
                                                       t1 = List.tail p1 |> fromJust
                                                       t2 = List.tail p2 |> fromJust
                                                   in
                                                       go t1 t2 <| accum ++ [h1]
                                                   else { common = fromList <| List.reverse accum
                                                        , sibFrom = Just h1
                                                        , sibTo = Just h2
                                                        , tailFrom = PF <| List.reverse <| fromJust <| List.tail p1
                                                        , tailTo = PF <| List.reverse <| fromJust <| List.tail p2
                                                        , fragFrom = PF <| List.reverse p1
                                                        , fragTo = PF <| List.reverse p2
                                                        }
                             (h1, h2) -> { common = fromList <| List.reverse accum
                                          , sibFrom = h1
                                          , sibTo = h2
                                          , tailFrom = PF <| List.reverse <| Maybe.withDefault [] <| List.tail p1
                                          , tailTo = PF <| List.reverse <| Maybe.withDefault [] <| List.tail p2
                                          , fragFrom = PF <| List.reverse p1
                                          , fragTo = PF <| List.reverse p2
                                          }
    in
        go (List.reverse (toList p1)) (List.reverse (toList p2)) []

isFragEmpty : PathFragment -> Bool
isFragEmpty frag = frag == PF []

shiftOne : Path -> PathFragment -> (Path, PathFragment)
shiftOne p (PF pf) =
    case pf of
        [] -> (p, PF pf) -- TODO: this is a bogus case, should be a crash
        otherwise ->
            case p of
                RootPath -> (Path (List.Extra.last pf |> fromJust) [],
                             PF (List.Extra.init pf |> fromJust))
                Path h2 t2 -> (Path (List.Extra.last pf |> fromJust) (h2 :: t2),
                               PF (List.Extra.init pf |> fromJust))

allCombos : Path -> PathFragment -> List Path
allCombos path1 frag1 =
    let
        go path frag accum = case shiftOne path frag of
                                 (p, PF []) -> p :: accum
                                 (p, pf) -> go p pf (p :: accum)
    in
        case frag1 of
            PF [] -> [] -- TODO: testing the base case twice = code smell
            otherwise -> go path1 frag1 []

advance : Path -> Path
advance p =
    case p of
        RootPath -> RootPath
        Path h t -> Path (h+1) t
