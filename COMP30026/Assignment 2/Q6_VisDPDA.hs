module VisDPDA
where

import Data.List

import Data.GraphViz
import Data.GraphViz.Attributes.Complete(Attribute(RankDir),RankDir(FromLeft))

import DPDA
import Hidden (sizeOfDPDA, checkDPDA)

-- ----------------------------
-- The PDA visualiser front end
-- ----------------------------

-- | Save a visualisation of `pda` to file **dpda.png**.
visDPDA :: DPDA -> IO ()
visDPDA
  = visDPDANamed "dpda"

-- | Save a visualisation of `pda` to file **`name`.png**.
visDPDANamed :: String -> DPDA -> IO ()
visDPDANamed name pda
  = do
      putStrLn "Checking DPDA..."
      checkDPDA pda
      putStrLn $ "Counting states: " ++ show (sizeOfDPDA pda)
      putStrLn "Drawing DPDA..."
      -- run GraphViz to layout the graph and save it as an image.
      addExtension (runGraphviz (dpdaToDot pda)) Png name
      putStrLn $ "PDA drawn and saved as '" ++ name ++ ".png'"


-- --------------------------
-- The PDA visualiser backend
-- --------------------------

-- | Convert an NFA to a GraphViz graph specification:
dpdaToDot :: DPDA -> DotGraph Integer
dpdaToDot (qs, xs, ys, ts, q0, qAs)
  = graphElemsToDot params nodes edges
    where
      -- invisible pre-start 'state' for start arrow:
      q0_ = (minimum qs) - 1
      preStartNode = (q0_, "")
      preStartEdge = (q0_, q0, "")

      -- Graph nodes:
      -- a list of nodes and their labels (including invisible pre-start
      -- 'node' with no label)
      nodes
        = preStartNode : pdaNodes qs
      -- Graph edges:
      -- a list of edges and their labels (including an edge from the
      -- invisible pre-start 'node' to the start state's node)
      edges
        = preStartEdge : pdaEdges ts
      
      -- GraphViz formatting and layout parameters:
      -- Node formatting function:
      -- show nodes as labelled circles (reject states), double circles
      -- (accept states) or no circles (invisible pre-start node)
      customNodeFmt (q, l)
        = [toLabel l, shape (stateShape q)]
        where
          stateShape q
            | q == q0_     = PlainText    -- invisible pre-start states
            | q `elem` qAs = DoubleCircle -- accept states
            | otherwise    = Circle       -- non-accept states
      -- Edge formatting function:
      -- show edges as labelled transition arrows
      customEdgeFmt (_, _, l)
        = [toLabel l, arrowTo vee]
      -- Other formatting and layout attributes:
      globalAttrs
        = [ GraphAttrs [RankDir FromLeft], NodeAttrs  [], EdgeAttrs  []]
      -- params :: GraphvizParams Int String String () String
      params
        = nonClusteredParams
            { globalAttributes = globalAttrs
            , isDirected       = True
            , fmtNode          = customNodeFmt
            , fmtEdge          = customEdgeFmt
            }

-- pdaNodes
-- Convert a list of states into a list of
-- (state, state-name) pairs
pdaNodes :: [State] -> [(Integer, String)]
pdaNodes qs
  = [(q, show q)| q <- qs]

-- pdaEdges
-- Convert a transition function into a list of
-- (from-state, to-state, edge-label) triples
pdaEdges :: [Transn] -> [(Integer, Integer, String)]
pdaEdges ts
  = [ (q, r, intercalate "\n" (labels q r)) | (q, r) <- edges ]
  where
    edges
      = nub [ (q, r) | ((q, _, _), (r, _)) <- ts ]
    labels q r
      = sort [ label x y z | ((q',x,y),(r',z)) <- ts, q'==q, r'==r ]

label :: Symbol -> StackSym -> StackSym -> String
label x y z
  = x:',':' ':y:'→':z:""
