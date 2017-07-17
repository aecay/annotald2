module TreeEdit.ZipperExts exposing (unzip, childZippers, updateDatum, zipper, goToRoot_)

import TreeEdit.Utils exposing (fromJust)

import Maybe
import List

import MultiwayTree exposing (Tree, children)
import MultiwayTreeZipper exposing (Zipper, goUp, goToChild)

-- All trees must have a root, so this is a nicer API than the original
-- function.  Errors in the structure of the zipper are runtime errors, not
-- hidden inside a Maybe.  TODO: make the error message from fromJust specific
-- to this function
goToRoot_ : Zipper a -> Zipper a
goToRoot_ ( tree, breadcrumbs ) =
    case breadcrumbs of
        [] ->
            ( tree, breadcrumbs )

        otherwise ->
            goUp ( tree, breadcrumbs ) |> Maybe.map goToRoot_ |> fromJust

unzip : Zipper a -> Tree a
unzip z =
    let
        (tree, _) = z
    in
        tree

childZippers : Zipper a -> List (Zipper a)
childZippers z =
    let
        (tree, _) = z
        nthChildZipper : Zipper a -> Int -> Zipper a
        nthChildZipper z n = fromJust <| goToChild n z
    in
        List.range 0 (List.length (children tree) - 1) |>
        List.map (nthChildZipper z)

-- doChildren : (Zipper a -> b) -> Zipper a -> List b
-- doChildren = goToFirstChild >>> perform f >>> doRights
-- doRights = perform f if not Nothing >>> goRight >>> doRights
-- pitfall: f must not move zipper (or must move it back)

updateDatum : (a -> a) -> Zipper a -> Zipper a
updateDatum fn ( MultiwayTree.Tree datum children, breadcrumbs ) =
    (MultiwayTree.Tree (fn datum) children, breadcrumbs)

zipper : Tree a -> Zipper a
zipper t = (t, [])
