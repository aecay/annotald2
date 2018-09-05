module TreeEdit.Tree.Type exposing ( TraceType(..)
                                   , ECType(..)
                                   , TreeInfo
                                   , Terminal (..)
                                   , Label
                                   , Metadata
                                   , Tree
                                   , Forest
                                   , info
                                   , children
                                   , label
                                   , index
                                   , metadata
                                   , private
                                   , constants
                                   , root
                                   )

import Array exposing (Array)
import Dict exposing (Dict)

import Monocle.Lens as Lens exposing (Lens)

import TreeEdit.Index as Index
import TreeEdit.OrderedDict exposing (OrderedDict)

type TraceType = Wh | Extraposition | Clitic

type ECType = Pro | Con | Exp | Star | Zero

type Terminal = Ordinary String MaybeIndex |
    Trace TraceType AlwaysIndex |
    Comment String |
    EmptyCat ECType MaybeIndex

type alias Info a = { a | label: Label
                    , metadata: Metadata
                    }

type alias AlwaysIndex = Info { index : Int }
type alias MaybeIndex = Info { index : Maybe Index.Index }
type alias NoIndex = Info {}

type Tree = TerminalOuter Terminal |
    Nonterminal (Array Tree) MaybeIndex

type alias Label = String

type alias Metadata = Dict String String

type alias TreeInfo = MaybeIndex

type alias Id = String

type alias Forest = OrderedDict Id Tree

constants :
    { comment : Tree
    , con : Tree
    , czero : Tree
    , pro : Tree
    , vb : Tree
    }
constants =
    { pro = TerminalOuter <| EmptyCat Pro { label = "NP-SBJ"
                                          , metadata = Dict.empty
                                          , index = Nothing
                                          }

    , con = TerminalOuter <| EmptyCat Con { label = "NP-SBJ"
                                          , metadata = Dict.empty
                                          , index = Nothing
                                          }
    , czero = TerminalOuter <| EmptyCat Zero { label = "C"
                                             , metadata = Dict.empty
                                             , index = Nothing
                                             }
    , comment = TerminalOuter <| Comment "XXX"
    , vb = TerminalOuter <| EmptyCat Star { label = "VVFIN"
                                          , metadata = Dict.empty
                                          , index = Nothing
                                          }
    }

private =
    let
        t label children = Nonterminal (Array.fromList children) <|
                           { label = label, metadata = Dict.empty, index = Nothing }
        ta label children = Nonterminal children <|
                            { label = label, metadata = Dict.empty, index = Nothing }
        l label text = TerminalOuter <| Ordinary text { label = label, metadata = Dict.empty, index = Nothing }
        trace typ label index = TerminalOuter <| Trace typ { label = label , index = index , metadata = Dict.empty}
    in
        { ordinary = Ordinary
        , trace = Trace
        , comment = Comment
        , emptycat = EmptyCat
        , terminalouter = TerminalOuter
        , nonterminal = Nonterminal
        , pro = Pro
        , con = Con
        , exp = Exp
        , star = Star
        , zero = Zero
        , wh = Wh
        , extraposition = Extraposition
        , clitic = Clitic
        , t = t
        , ta = ta
        , l = l
        , makeTrace = trace
        , either = either
        }

info : Lens Tree MaybeIndex
info =
    let
        get t =
            case t of
                Nonterminal _ info -> info
                TerminalOuter (Ordinary _ info) -> info
                TerminalOuter (Trace _ info) -> { label = info.label
                                                , metadata = info.metadata
                                                , index = Just <| Index.normal info.index
                                                }
                TerminalOuter (Comment _) -> { label = "CODE"
                                             , metadata = Dict.empty
                                             , index = Nothing
                                             }
                TerminalOuter (EmptyCat _ info) -> info
        set : MaybeIndex -> Tree -> Tree
        set i t =
            case t of
                Nonterminal children _ -> Nonterminal children i
                TerminalOuter (Ordinary text _) -> TerminalOuter <| Ordinary text i
                TerminalOuter (Trace typ info) ->
                    let
                        newidx : Int
                        newidx = i.index |> Maybe.map (.get Index.number) |> Maybe.withDefault info.index
                        newInfo = { label = i.label
                                  , metadata = i.metadata
                                  , index = newidx
                                  }
                    in
                        TerminalOuter <| Trace typ newInfo
                TerminalOuter (Comment com) -> Debug.log "bogus attempt to set info of comment node" <|
                                               TerminalOuter <| Comment com
                TerminalOuter (EmptyCat typ _) -> TerminalOuter <| EmptyCat typ i
    in
        Lens get set

children : Lens Tree (Array Tree)
children =
    let
        get t =
            case t of
                Nonterminal c _ -> c
                _ -> Array.empty
        set c t =
            case t of
                Nonterminal _ info -> Nonterminal c info
                _ -> t
    in
        Lens get set

-- This is a lens to a Maybe instead of an optional because we need to be able
-- to remove the index entirely by setting it to Nothing
index : Lens Tree (Maybe Index.Index)
index =
    let
        inner : Lens TreeInfo (Maybe Index.Index)
        inner = Lens .index (\i r -> { r | index = i })
    in
        Lens.compose info inner

label : Lens Tree String
label =
    let
        get t = info.get t |> .label
        set s t =
            let
                i = info.get t
            in
                info.set { i | label = s } t
    in
        Lens get set

metadata : Lens Tree Metadata
metadata =
    let
        inner = Lens .metadata (\m t -> { t | metadata = m })
    in
        Lens.compose info inner

either : (Terminal -> a) -> (TreeInfo -> Array Tree -> a) -> Tree -> a
either terminalF nonterminalF tree =
    case tree of
        TerminalOuter terminal -> terminalF terminal
        Nonterminal children info -> nonterminalF info children

root : Id -> Tree
root id = TerminalOuter <| EmptyCat Star { label = "XXX"
                                         , metadata = Dict.fromList [ ("ID", id) ]
                                         , index = Nothing
                                         }
