---------------------------------------------------------------------------------
-- | Lévy distribution functions
--
-- >>> import System.Random (newStdGen)
-- >>> import Control.Monad.Random
--
-- >>> test :: IO ()
-- >>> test = do
-- >>>   gen <- newStdGen
-- >>>   let x = evalRandT (sampleLevy 0.0 0.5) gen :: Maybe Double
-- >>>   putStrLn $ show x
---------------------------------------------------------------------------------

module Statistics.Distribution.Levy (
    levyCdf
  , levyPdf
  , sampleLevy
  ) where

import           Control.Monad              (MonadPlus, mzero)
import           Control.Monad.Random       (RandT, RandomGen, evalRandT)
import           Control.Monad.Random.Class (getRandom)
import           Data.Number.Erf            (Erf, InvErf, erfc, invnormcdf)
import           System.Random              (Random)


-- | Compute the probability density at x for the Lévy distribution
--   using the given location offset and scale params
--   n.b. x must be >= offset
levyPdf
  :: (MonadPlus m, Floating a, Ord a)
  => a
  -- ^ domain location offset parameter
  -> a
  -- ^ scale parameter
  -> a
  -- ^ x
  -> m a
levyPdf offset scale x
  | x >= offset = return $ sqrt (scale / (2 * pi)) *
                           (exp (negate (scale / (2 * (x - offset)))) /
                            ((x - offset) ** (3 / 2)))
  | otherwise   = mzero

-- | Compute the cumulative probability at x for the Lévy distribution
--   using the given location offset and scale params
--   n.b. x must be >= offset
levyCdf
  :: (MonadPlus m, Erf a, Ord a)
  => a
  -- ^ domain location offset parameter
  -> a
  -- ^ scale parameter
  -> a
  -- ^ x
  -> m a
levyCdf offset scale x
  | x >= offset = return . erfc . sqrt $ scale / (2 * (x - offset))
  | otherwise   = mzero

-- | Sample the Lévy distribution using the inverse transform method
--   using the given location offset and scale params
sampleLevy
  :: (RandomGen g, MonadPlus m, InvErf a, Ord a, Random a)
  => a
  -- ^ domain location offset parameter
  -> a
  -- ^ scale parameter
  -> RandT g m a
sampleLevy offset scale = do
  u <- getRandom
  return $ offset + (scale /
                     (invnormcdf (1 - (u / 2)) ** 2))
