module Utils exposing (enumerate
                      , zip
                      , fromJust
                      , (?>)
                      , (?>?)
                      , all
                      , remove
                      , insert
                      , do
                      , maybeDo
                      )

import List

import Debug

import Monocle.Lens exposing (Lens)
-- import Monocle.Common exposing (array, (=>))

-- import Tree exposing (Tree)
-- import Model exposing (Model, TreeLens)

zip : List a -> List b -> List (a, b)
zip = List.map2 (,)

enumerate : List a -> List (Int, a)
enumerate a = zip (List.range 0 <| List.length a - 1) a

remove : Int -> List a -> List a
remove i list =
    List.take i list ++ List.drop (i+1) list

insert : Int -> a -> List a -> List a
insert i x list =
    List.take i list ++ x :: List.drop i list

do : Lens a b -> (b -> b) -> a -> a
do lens f a = lens.get a |> f |> flip lens.set a

maybeDo : Lens a b -> (b -> Maybe b) -> a -> a
maybeDo lens f a =
    lens.get a |> f |> Maybe.withDefault (lens.get a) |> flip lens.set a

-- toList : Array a -> List a
-- toList = foldr (::) []

-- enumerateChildren : List Int -> List b -> List (List Int, b)
-- enumerateChildren indices children =
--     let augmentedIndices = map (\x -> indices ++ [x]) (range 0 <| length children - 1)
--     in zip augmentedIndices children

-- enumerateLenses : TreeLens -> Tree -> Array (TreeLens, Tree)
-- enumerateLenses p tree =
--     case tree of
--         Tree.Terminal _ -> Debug.crash "Should never happen"
--         Tree.Nonterminal t -> map (\(i, c) -> (p => children => array i, c)) <|
--                               enumerate t.children

-- TODO: move to Trees, or new Trees.Lenses library

-- nonterminalOptional : (Tree.Nonterminal_ -> a) ->
--                       (a -> Tree.Nonterminal_ -> Tree.Nonterminal_) ->
--                       Optional Tree a
-- nonterminalOptional get_ set_ =
--     let
--         get : Tree -> Maybe a
--         get x = case x of
--                     Tree.Terminal _ -> Nothing
--                     Tree.Nonterminal t -> Just (get_ t)
--         set : a -> Tree -> Tree
--         set y x = case x of
--                       Tree.Terminal _ -> x
--                       Tree.Nonterminal t -> Tree.Nonterminal (set_ y t)
--     in
--         Optional get set

-- bothOptional : (Tree.Nonterminal_ -> a) ->
--                (Tree.Terminal_ -> a) ->
--                (a -> Tree.Nonterminal_ -> Tree.Nonterminal_) ->
--                (a -> Tree.Terminal_ -> Tree.Terminal_) ->
--                Optional Tree a
-- bothOptional getNt getT setNt setT =
--     let
--         get : Tree -> Maybe a
--         get x = case x of
--                     Tree.Terminal t -> Just (getT t)
--                     Tree.Nonterminal t -> Just (getNt t)
--         set : a -> Tree -> Tree
--         set y x = case x of
--                       Tree.Terminal t -> Tree.Terminal (setT y t)
--                       Tree.Nonterminal t -> Tree.Nonterminal (setNt y t)
--     in
--         Optional get set

-- children : Optional Tree (Array Tree)
-- children = nonterminalOptional .children (\c x -> { x | children = c })

-- selected : Optional Tree Bool
-- selected = bothOptional
--            .selected
--            .selected
--            (\s x -> { x | selected = s })
--            (\s x -> { x | selected = s })

-- rootLens : Optional Model Tree
-- rootLens =
--     let
--         get : Model -> Maybe Tree
--         get = .root >> Just
--         set : Tree -> Model -> Model
--         set r m =
--             case r of
--                 Tree.Terminal _ -> Debug.crash "Should never happen"
--                 Tree.Nonterminal t -> { m | root = r }
--     in Optional get set

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

(?>) : Maybe a -> (a -> b) -> Maybe b
(?>) x f = Maybe.map f x

infixl 0 ?>

-- TODO: remove
(?>?) : Maybe a -> (a -> Maybe b) -> Maybe b
(?>?) = flip Maybe.andThen

infixl 0 ?>?

all : (a -> Bool) -> List a -> Bool
all f l = List.foldl (&&) True <| List.map f l

-- This is just Maybe.andThen
-- (?>>) : Maybe a -> (a -> Maybe b) -> Maybe b
-- (?>>) v f = case v of
--                 Nothing -> Nothing
--                 Just vv -> (f vv)

-- This is just Maybe.map
-- (?>>>) : Maybe a -> (a -> b) -> Maybe b
-- (?>>>) v f = case v of
--                 Nothing -> Nothing
--                 Just vv -> Just (f vv)
