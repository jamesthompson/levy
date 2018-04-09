# Lévy

Lévy distribution functions.

## Requirements

This package builds on [MonadRandom](https://hackage.haskell.org/package/MonadRandom) and [random](https://hackage.haskell.org/package/random).

### Example usage

```
import System.Random (newStdGen)
import Control.Monad.Random

test :: IO ()
test = do
  gen <- newStdGen
  let x = evalRandT (sampleLevy 0.0 0.5) gen :: Maybe Double
  putStrLn $ show x
```

### Building

This package is built with nix, a `default.nix` file is included for your convenience.
