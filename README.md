# markov-gen

A simple library for generating Markov sequences in Haskell.

## Usage

Uses the `RVar` monad from the `random-fu` library.

```haskell
import Markovgen
import Data.Random

lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam accumsan erat vitae ex vulputate vestibulum. Curabitur vehicula justo id nisi posuere, ut tincidunt ante sagittis. Nullam sed tincidunt augue. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Ut a neque sit amet eros pretium lobortis. Nam faucibus id tellus et consectetur. Vivamus et elementum enim, quis gravida nisi. Quisque gravida pellentesque orci, non ultricies eros rutrum fringilla. Aenean dapibus dignissim blandit."

-- Generates and prints 10 random latin words generated with state length 2
loremtest :: IO ()
loremtest = do
  let wordlist = words lorem
  let wordgraph = train 2 ' ' wordlist
  chains <- sample $ replicateM 10 $ randomChain 2 ' ' wordgraph
  putStrLn "Random Words: "
  print chains
```
