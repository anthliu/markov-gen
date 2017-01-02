import Markovgen
import qualified Data.Text as T
import qualified Data.Text.IO as TO
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import Data.Random
import Control.Monad

lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam accumsan erat vitae ex vulputate vestibulum. Curabitur vehicula justo id nisi posuere, ut tincidunt ante sagittis. Nullam sed tincidunt augue. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Ut a neque sit amet eros pretium lobortis. Nam faucibus id tellus et consectetur. Vivamus et elementum enim, quis gravida nisi. Quisque gravida pellentesque orci, non ultricies eros rutrum fringilla. Aenean dapibus dignissim blandit."

loremtest :: IO ()
loremtest = do
  let wordlist = words lorem
  putStrLn $ "Testing words: " ++ show wordlist
  let wordgraph = train 2 ' ' wordlist
  result <- sample $ replicateM 10 $ randomEdge ((unNodes wordgraph ) M.! (Seq.fromList "  "))
  putStrLn "Random Edges: "
  print result
  chains <- sample $ replicateM 10 $ randomChain 2 ' ' wordgraph
  putStrLn "Random Words: "
  print chains

othello :: IO T.Text
othello = TO.readFile "test/othello.txt"

genothello :: IO ()
genothello = do
  let wlen = 1
      term = T.pack " "
  t <- othello
  let graph = train wlen term . filter ((>3) . length) . fmap T.words . T.lines $ t
  putStrLn "Random Othello lines"
  chains <- sample $ replicateM 10 $ randomChain wlen term graph
  TO.putStrLn $ T.unlines $ map T.unwords chains

main :: IO ()
main = do
  loremtest
  genothello
