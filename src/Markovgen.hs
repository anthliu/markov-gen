{-# LANGUAGE OverloadedStrings #-}

module Markovgen
    ( Nodes
    , Edges
    , makeMChain
    , someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Arrow ((***))
import Data.Sequence as Seq
import Data.List (foldl')
import Data.Function (on)
import Data.Random

newtype Edges a = Edges {unEdges :: Map.Map a Int}

newtype Nodes a = Nodes { unNodes :: Map.Map (Seq.Seq a) (Edges a) }

makeMChain :: (Ord a) => Int -> a -> [a] -> Nodes a
makeMChain n term = Nodes . lastchain . foldl' go (Map.empty, Seq.replicate n term)
  where
    combineEdges = ((.) . (.)) Edges $ on (Map.unionWith (+)) unEdges
    updateNodes chain n = 
      Nodes . Map.insertWith combineEdges chain (Edges $ Map.singleton n 1) . unNodes
    lastchain (mp, last) = mp
    go = const

someFunc :: IO ()
someFunc = putStrLn "someFunc"
