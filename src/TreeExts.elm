module TreeExts exposing (setChild, updateDatum, updateChildren)

-- TODO: Wrap all necessary fns from MultiwayTree so that files in this
-- project only need to import this library

import MultiwayTree exposing (Tree(..))

import Res as R

setChild : Int -> Tree a -> Tree a -> R.Result (Tree a)
setChild i new parent =
    case parent of
        Tree datum children -> if List.length children > i
                               then R.succeed <| Tree datum <|
                                   List.take i children ++
                                   [new] ++
                                   List.drop (i + 1) children
                               else R.fail "TX.setChild"

updateDatum : (a -> a) -> Tree a -> Tree a
updateDatum f t =
    case t of
        Tree datum children -> Tree (f datum) children

updateChildren : (List (Tree a) -> List (Tree a)) -> Tree a -> Tree a
updateChildren f t =
    case t of
        Tree datum children -> Tree datum (f children)
