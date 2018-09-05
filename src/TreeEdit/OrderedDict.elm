module TreeEdit.OrderedDict exposing ( OrderedDict
                                     , empty
                                     , fromList
                                     , insertAt
                                     , remove
                                     , get
                                     , values
                                     , keys
                                     , update
                                     , toArray
                                     , toList
                                     , elemIndex
                                     )

import Dict exposing (Dict)
import Tuple

import Array exposing (Array)

import TreeEdit.Utils as Utils exposing (fromJust)

type OrderedDict comparable value = OD (Dict comparable value) (Array comparable)

empty : OrderedDict comparable v
empty = OD Dict.empty <| Array.empty

insert : comparable -> val -> OrderedDict comparable val -> OrderedDict comparable val
insert k v (OD d l) =
    if Dict.member k d
    then OD (Dict.insert k v d) l
    else OD (Dict.insert k v d) <| Array.push k l

remove : comparable -> OrderedDict comparable val -> OrderedDict comparable val
remove k (OD d l) = OD (Dict.remove k d) (Array.filter (\x -> x /= k) l)

update : comparable -> (Maybe val -> Maybe val) -> OrderedDict comparable val -> OrderedDict comparable val
update k fn (OD d l) =
    let
        newval = fn <| Dict.get k d
        newlist = if newval == Nothing
                  then Array.filter (\x -> x /= k) l
                  else
                      if Dict.member k d
                      then l
                      else Array.push k l
    in
        OD (Dict.update k (\_ -> newval) d) newlist

get : comparable -> OrderedDict comparable val -> Maybe val
get k (OD d l) = Dict.get k d

insertAt : Int -> comparable -> val -> OrderedDict comparable val -> OrderedDict comparable val
insertAt idx key val (OD d l) =
    if Dict.member key d
    then OD d l -- TODO: seems like an error...or should we move it to this
                -- index?
    else
        OD (Dict.insert key val d) (Utils.insert idx key l)

values : OrderedDict comparable val -> Array val
values (OD d _) =
    Dict.values d |> Array.fromList

keys : OrderedDict comparable val -> Array comparable
keys (OD _ l) = l

toArray : OrderedDict comparable val -> Array (comparable, val)
toArray (OD d l) = Dict.toList d |> Array.fromList

toList : OrderedDict comparable val -> List (comparable, val)
toList (OD d l) = Dict.toList d

member : comparable -> OrderedDict comparable val -> Bool
member k (OD d l) = Dict.member k d

fromList : List (comparable, val) -> OrderedDict comparable val
fromList l = OD (Dict.fromList l) (List.map Tuple.first l |> Array.fromList)

elemIndex : comparable -> OrderedDict comparable val -> Maybe Int
elemIndex e (OD _ l) =
    Array.indexedMap (,) l |>
    Array.filter (\(i,x) -> x == e) |>
    Array.map Tuple.first |>
    Array.get 0
