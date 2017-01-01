{-# LANGUAGE OverloadedStrings #-}

module Markovgen
    ( Nodes
    , unNodes
    , Edges
    , unEdges
    , singleEdge
    , singleNode
    , trainExample
    , train
    , randomEdge
    , randomChain
    , someFunc
    ) where

import qualified Data.Map as Map
import Data.Monoid
import Control.Monad.State
import Data.Random
import Data.Sequence as Seq
import Data.List (foldl', foldl1', mapAccumL)

newtype Edges a = Edges { unEdges :: Map.Map a Int }

newtype Nodes a = Nodes { unNodes :: Map.Map (Seq.Seq a) (Edges a) }

instance (Ord a) => Monoid (Edges a) where
  mempty = Edges Map.empty
  mappend (Edges a) (Edges b) = Edges $ Map.unionWith (+) a b

instance (Ord a) => Monoid (Nodes a) where
  mempty = Nodes Map.empty
  mappend (Nodes a) (Nodes b) = Nodes $ Map.unionWith mappend a b

singleEdge :: a -> Edges a
singleEdge = Edges . flip Map.singleton 1

singleNode :: Seq.Seq a -> a -> Nodes a
singleNode chain = Nodes . Map.singleton chain . singleEdge

buildTraining :: Int -> a -> [a] -> [(Seq.Seq a, a)]
buildTraining n term = flip evalState (Seq.replicate n term) . traverse go . (++ [term])
  where
    go :: (Monad m) => a -> StateT (Seq.Seq a) m (Seq.Seq a, a)
    go x = state $ \chain -> ((chain, x), Seq.drop 1 . (Seq.|> x) $ chain)

trainExample :: (Ord a) => Int -> a -> [a] -> Nodes a
trainExample n term = mconcat . fmap (uncurry singleNode) . buildTraining n term

train :: (Ord a) => Int -> a -> [[a]] -> Nodes a
train n term = mconcat . fmap (trainExample n term)

randomEdge :: Edges a -> RVar a
randomEdge (Edges e) = do
  let edges = Map.toList e
  let (total, weights) = mapAccumL (\x y -> (x + snd y, fmap (+x) y)) 0 edges
  result <- uniform 1 total
  return $ fst . head . dropWhile ((< result) . snd) $ weights

randomChain :: (Ord a) => Int -> a -> Nodes a -> RVar [a]
randomChain n term (Nodes graph) = go $ Seq.replicate n term
  where
    go xs = do
      result <- randomEdge $ graph Map.! xs
      if result == term
        then return []
        else fmap (result:) $ go (Seq.drop 1 . (Seq.|> result) $ xs)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
