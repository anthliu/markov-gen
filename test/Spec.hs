import Markovgen
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import Data.Random
import Control.Monad

main :: IO ()
main = do
  let wordlist = words "the quick fox jumped over the lazy dog"
  putStrLn $ "Testing words: " ++ show wordlist
  let wordgraph = train 2 ' ' wordlist
  print wordgraph
  result <- sample $ replicateM 10 $ randomEdge (wordgraph M.! (Seq.fromList "  "))
  print result
