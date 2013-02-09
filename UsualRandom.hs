module UsualRandom where

import System.Random

-- | split is said to be not that good,
--   there must be a more effective solution to distribute randomness between functions
iterateRandomGen :: RandomGen g => g -> [g]
iterateRandomGen g = map fst $ iterate split' (g,g)
    where split' = split . snd

-- | returns INFINITE list !
iterateRandom :: RandomGen g => g -> (g -> a) -> [a]
iterateRandom g f = map f (iterateRandomGen g)

mapWithRandom :: RandomGen g => g -> (g -> a -> b) -> [a] -> [b]
mapWithRandom g f = zipWith ($) (applyRandom g f) 

mutateGenomeWith :: RandomGen g => (g -> a -> a) -> g -> [a] -> [a]
mutateGenomeWith g = mapWithRandom g f