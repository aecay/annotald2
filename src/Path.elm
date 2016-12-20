module Path exposing ( Path(..)
                     , PathFragment(..)
                     , singleton
                     , childPath
                     , root
                     , parent
                     , splitCommon
                     , lessThan
                     , moveRight
                     , moveLeft
                     , isFragSingleton
                     , join
                     , isFragEmpty
                     , areFragsAdjacent
                     , shiftOne
                     , foot
                     , allCombos
                     , unwrap
                     )

-- import Monocle.Lens exposing (Lens, compose)
import List.Extra

-- import Tree exposing (Tree)
import Utils exposing (fromJust)

type Path = Path Int (List Int) | RootPath

type PathFragment = PF (List Int)

-- checkLength : Int -> Tree -> Tree
-- checkLength i tree =
--     if List.length tree.children < i - 1
--     then Debug.crash "Bad list index"
--     else tree

-- unsafeGet : Int -> List a -> a
-- unsafeGet i list =
--     list !! i |> fromJust

-- childPath : Int -> Path a -> Path a
-- childPath idx { path, root } =
--     let
--         get tree = (checkLength tree).children !! idx
--         set newChild tree = { (checkLength tree) | children = insert idx newChild }
--     in
--         { path = compose path <| Lens get set
--         , root = root
--         , foot = idx
--         }

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
        RootPath -> Debug.crash "Can't get the root of the root!"
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

splitCommon : Path -> Path -> (Path, PathFragment, PathFragment)
splitCommon p1 p2 =
    let
        go p1 p2 accum = case (p1, p2) of
                             (h1 :: t1, h2 :: t2) -> if h1 == h2
                                                     then go t1 t2 <| accum ++ [h1]
                                                     else (accum, p1, p2)
                             otherwise -> (accum, p1, p2)
        finalize (head, t1, t2) = ( fromList (List.reverse head)
                                  , PF (List.reverse t1)
                                  , PF (List.reverse t2)
                                  )
    in
        finalize <| go (List.reverse (toList p1))
            (List.reverse (toList p2)) []

lessThan : Path -> Path -> Bool
lessThan p1 p2 = List.reverse (toList p1) < List.reverse (toList p2)

-- All these functions are for implementing movement

moveRight : PathFragment -> PathFragment
moveRight (PF pf) =
    let
        len = List.length pf - 1
        init = List.take len pf
        tail = List.map ((+) 1) <| List.drop len pf
    in
        PF <| init ++ tail

moveLeft : PathFragment -> PathFragment
moveLeft (PF pf) =
    let
        len = List.length pf - 1
        init = List.take len <| Debug.log "pf" pf
        tail = List.map (flip (-) 1) <| Debug.log "tail" <| List.drop len pf
    in
        Debug.log "left" <| PF <| init ++ tail

isFragSingleton : PathFragment -> Bool
isFragSingleton (PF pf) = List.length pf == 1

areFragsAdjacent : PathFragment -> PathFragment -> Bool
areFragsAdjacent (PF f1) (PF f2) =
    let
        tail1 = List.Extra.last f1
        tail2 = List.Extra.last f2
        cmp a b = a == b - 1
    in
        Maybe.map2 cmp tail1 tail2 |> Maybe.withDefault False

isFragEmpty : PathFragment -> Bool
isFragEmpty frag = frag == PF []

shiftOne : Path -> PathFragment -> (Path, PathFragment)
shiftOne p (PF pf) =
    case pf of
        [] -> (p, PF pf) -- TODO: this is a bogus case, should be a crash
        otherwise ->
            case p of
                RootPath -> (Path (List.Extra.last pf |> Utils.fromJust) [],
                             PF (List.Extra.init pf |> Utils.fromJust))
                Path h2 t2 -> (Path (List.Extra.last pf |> Utils.fromJust) (h2 :: t2),
                               PF (List.Extra.init pf |> Utils.fromJust))

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

-- TODO: this is a code smell, and also redundant if we're exporting PF anyway
-- (smellier)
unwrap : PathFragment -> List Int
unwrap p =
    case p of
        PF x -> x
