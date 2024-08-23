import Data.List

-- Data.GraphViz:
-- a third party Haskell package wrapping the GraphViz suite
-- of Graph Visualisation tools:
-- GraphViz tools:   https://graphviz.org/
-- GraphViz wrapper: https://hackage.haskell.org/package/graphviz
import Data.GraphViz
import Data.GraphViz.Attributes.Complete(Attribute(RankDir),RankDir(FromLeft))

import DFA



-- --------------------------------------
--
--  A DFA visualiser written in Haskell.
--
-- --------------------------------------

-- visDFA
-- Save a visualisation of `dfa` to file 'dfa.png'
visDFA :: DFA -> IO ()
visDFA
  = visDFANamed "dfa"


-- visDFANamed
-- Save a visualisation of `dfa` to file '`name`.png'.
visDFANamed :: String -> DFA -> IO ()
visDFANamed name dfa
  = do
      -- make sure the DFA is well-formed
      putStrLn "Checking DFA..."
      checkDFA dfa

      putStrLn "Drawing DFA..."
      -- convert the DFA to GraphViz's DOT format
      let dfaDotGraph = dfaToDot dfa
      -- run GraphViz to layout the graph and save it as
      -- an image.
      addExtension (runGraphviz dfaDotGraph) Png name
      putStrLn $ "DFA drawn and saved as '" ++ name ++ ".png'"


-- dfaToDot
-- Convert a DFA to a GraphViz graph specification:
dfaToDot :: DFA -> DotGraph Int
dfaToDot (qs, as, ts, q0, qa)
  = graphElemsToDot params nodes edges
    where
      -- Graph nodes:
      -- a list of nodes and their labels (including invisible pre-start
      -- 'node' with no label)
      nodes
        = (-1, "") : dfaNodes qs
      -- Graph edges:
      -- a list of edges and their labels (including an edge from the
      -- invisible pre-start 'node' to the start state's node)
      edges
        = (-1, q0, "") : dfaEdges ts
      
      -- GraphViz formatting and layout parameters:
      -- Node formatting function:
      -- show nodes as labelled circles (reject states), double circles
      -- (accept states) or no circles (invisible pre-start node)
      customNodeFmt (n, l)
        = [toLabel l, shape (stateShape n)]
        where
          stateShape n
            | n == -1     = PlainText    -- invisible pre-start state
            | n `elem` qa = DoubleCircle -- accept states
            | otherwise   = Circle       -- non-accept states
      -- Edge formatting function:
      -- show edges as labelled transition arrows
      customEdgeFmt (_, _, l)
        = [toLabel l, arrowTo vee]
      -- Other formatting and layout attributes:
      globalAttrs
        = [ GraphAttrs [RankDir FromLeft], NodeAttrs  [], EdgeAttrs  []]
      params :: GraphvizParams Int String String () String
      params
        = defaultParams { globalAttributes = globalAttrs
                        , isDirected       = True
                        , fmtNode          = customNodeFmt
                        , fmtEdge          = customEdgeFmt
                        }


-- --------------------------------------

-- TODO:
-- Complete the following two helper functions
-- to make the DFA visualiser work properly!

-- dfaNodes
-- Convert a list of states into a list of
-- (state, state-name) pairs
dfaNodes :: [State] -> [(Int, String)]
dfaNodes [] = []
dfaNodes (q:qs)
  = [(q, show q)] ++ dfaNodes qs

-- dfaEdges
-- Convert a transition function into a list of
-- (from-state, to-state, edge-label) triples
dfaEdges :: [Transn] -> [(Int, Int, String)]
dfaEdges delta
  =  nub [(start, dest, (combineEdgeLabels sameEdges)) 
     | sameEdges <- groupedEdges
     , ((start, dest), symbol) <- sameEdges
     ] 
  where 
    sortedEdges = sort [ ((start, dest), symbol) | ((start, symbol), dest) <- delta]
    groupedEdges = groupBy (\x y -> fst x == fst y) sortedEdges
    
    
combineEdgeLabels :: [((Int, Int), Char)] -> String
combineEdgeLabels [] = ""
combineEdgeLabels (((start, dest), symbol):xs)
  = [symbol] ++ combineEdgeLabels xs
    
