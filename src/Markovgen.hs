{-# LANGUAGE OverloadedStrings #-}

module Markovgen
    ( Nodes
    , Edges
    , trainExample
    , train
    , randomEdge
    , randomChain
    , someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Random
import Data.Sequence as Seq
import Data.List (foldl', foldl1', mapAccumL)
import Data.Function (on)

type Edges a = Map.Map a Int

type Nodes a = Map.Map (Seq.Seq a) (Edges a)

combineEdges :: (Ord a) => Edges a -> Edges a -> Edges a
combineEdges = Map.unionWith (+)

combineNodes :: (Ord a) => Nodes a -> Nodes a -> Nodes a
combineNodes = Map.unionWith combineEdges

trainExample :: (Ord a) => Int -> a -> [a] -> Nodes a
trainExample n term = lastchain . foldl' go (Map.empty, Seq.replicate n term)
  where
    updateNodes chain n = 
      Map.insertWith combineEdges chain (Map.singleton n 1)
    lastchain = fst . flip go term
    go (mp, chain) n = (updateNodes chain n mp, Seq.drop 1 . (Seq.|> n) $ chain)

train :: (Ord a) => Int -> a -> [[a]] -> Nodes a
train n term = foldl1' combineNodes . fmap (trainExample n term)

randomEdge :: Edges a -> RVar a
randomEdge e = do
  let edges = Map.toList e
  let (total, weights) = mapAccumL (\x y -> (x + snd y, fmap (+x) y)) 0 edges
  result <- uniform 1 total
  let (key, _) = head . dropWhile ((< result) . snd) $ weights
  return key

randomChain :: (Ord a) => Int -> a -> Nodes a -> RVar [a]
randomChain n term graph = go $ Seq.replicate n term
  where
    go xs = do
      result <- randomEdge $ graph Map.! xs
      if result == term
        then return []
        else fmap (result:) $ go (Seq.drop 1 . (Seq.|> result) $ xs)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
