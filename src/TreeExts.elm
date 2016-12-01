module TreeExts exposing (setChild, updateDatum)

-- TODO: Wrap all necessary fns from MultiwayTree so that files in this
-- project only need to import this library

import MultiwayTree exposing (Tree(..))

setChild : Int -> Tree a -> Tree a -> Maybe (Tree a)
setChild i new parent =
    case parent of
        Tree datum children -> if List.length children > i
                               then Just <| Tree datum <|
                                   List.take i children ++
                                   [new] ++
                                   List.drop (i + 1) children
                               else Nothing

updateDatum : (a -> a) -> Tree a -> Tree a
updateDatum f t =
    case t of
        Tree datum children -> Tree (f datum) children
