module Parser.DotGen (graphVizToDot) where

import Parser.GraphViz
import qualified Data.Map as Map
import Data.List (intercalate)

graphVizToDot :: GraphViz -> String
graphVizToDot gv =
  let
    -- Clustering: gather simple/highlevel place and transition names
    allTransitions = concatMap (\(_, lst) -> map fst lst) (Map.toList (transitionsGV gv))
    simplePlaces = Map.findWithDefault [] "simple" (places gv)
    allHLVars = filter (/= "simple") (Map.keys (places gv))
    hlPlacesByVar = \v -> Map.findWithDefault [] v (places gv)
    hlTransByVar = \v -> [ t | (t, _) <- Map.findWithDefault [] v (transitionsGV gv) ]

    -- Generate node declarations
    placesDecls names = unlines $
      ["node [shape=circle, width=0.5, style=filled, fillcolor=lightgray];"] ++
      [n ++ ";" | n <- names]

    transitionsDecls names = unlines $
      ["node [shape=rectangle, width=0.1, height=0.6, style=filled, fillcolor=black];"] ++
      [n ++ " [xlabel=\"" ++ n ++ "\", label=\"\"];" | n <- names]

    -- Cluster definition
    cluster :: String -> [String]
    cluster v =
        let ps = Map.findWithDefault [] v (places gv)
            ts = [ t | (t, _) <- Map.findWithDefault [] v (transitionsGV gv) ]
            nodes = ps ++ ts
        in if null nodes
            then []
            else
              [ "  subgraph cluster_" ++ v ++ " {"
              , "    style=filled;"
              , "    color=lightgrey;"
              , "    node [style=filled,color=white];"
              , "    " ++ unwords (map (++ ";") nodes)
              , "    label = \"" ++ v ++ "\";"
              , "  }"
              ]

    -- Arcs
    edge :: (String, String, String, String, Bool) -> String
    edge (src, tgt, weight, inh, isPlaceToTrans) =
        let dir = if isPlaceToTrans then "->" else "->"
            port = if isPlaceToTrans then "tailport=e" else "headport=w"
            labelHtml =
              if null inh
                then "<" ++ weight ++ ">"
                else "<<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"0\">" ++
                    "<TR><TD>" ++ weight ++ "</TD></TR>" ++
                    "<TR><TD><FONT POINT-SIZE=\"10\">" ++ inh ++ "</FONT></TD></TR>" ++
                    "</TABLE>>"
        in "  " ++ src ++ " " ++ dir ++ " " ++ tgt ++
          " [label=" ++ labelHtml ++ ", " ++ port ++ "];"


    dotLines =[ "digraph PetriNet {",
      "  rankdir=LR;",
      "  node [shape=circle, width=0.5, style=filled, fillcolor=lightgray];"
      ]
      ++ map (\p -> "  " ++ p ++ ";") (concat (Map.elems (places gv)))
      ++ [ "  node [shape=rectangle, width=0.1, height=0.6, style=filled, fillcolor=black];" ]
      ++ map (\t -> "  " ++ t ++ " [xlabel=\"" ++ t ++ "\", label=\"\"];" ) allTransitions
      ++ concatMap cluster allHLVars
      ++ map edge (arc gv)
      ++ [ "}" ]
  in
    unlines dotLines
