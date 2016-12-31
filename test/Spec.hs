import Markovgen
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import Data.Random
import Control.Monad

lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam accumsan erat vitae ex vulputate vestibulum. Curabitur vehicula justo id nisi posuere, ut tincidunt ante sagittis. Nullam sed tincidunt augue. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Ut a neque sit amet eros pretium lobortis. Nam faucibus id tellus et consectetur. Vivamus et elementum enim, quis gravida nisi. Quisque gravida pellentesque orci, non ultricies eros rutrum fringilla. Aenean dapibus dignissim blandit."

main :: IO ()
main = do
  let wordlist = words lorem
  putStrLn $ "Testing words: " ++ show wordlist
  let wordgraph = train 2 ' ' wordlist
  result <- sample $ replicateM 10 $ randomEdge (wordgraph M.! (Seq.fromList "  "))
  putStrLn "Random Edges: "
  print result
  chains <- sample $ replicateM 10 $ randomChain 2 ' ' wordgraph
  putStrLn "Random Words: "
  print chains
