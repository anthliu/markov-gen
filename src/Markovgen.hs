{-# LANGUAGE OverloadedStrings #-}

module Markovgen
    ( MChain
    , makeMChain
    , someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (foldl')
import Data.Random

newtype MChain a = MChain { unMChain :: Map.Map [a] Int }

makeMChain :: Int -> [a] -> MChain a
makeMChain n = MChain . foldl' go Map.empty
  where
    go = const

someFunc :: IO ()
someFunc = putStrLn "someFunc"
