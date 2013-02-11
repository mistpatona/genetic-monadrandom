{-# LANGUAGE TupleSections #-}

module GenDeterm
       (orderByFitness
       ,makeAllPairs
       ,reproductionSize
       ,signumT
	   )  where

import Control.Applicative ( (<*>) )
import Data.Function  (on)
import Data.List (sortBy, sort, subsequences)

-- | the algo will MINIMIZE (!) the fitness function
--                     f

orderByFitness :: Ord b => (a -> b) -> [a] -> [a]
orderByFitness f = sortBy (compare `on` f) 

makeAllPairs :: [a] -> [(a,a)]
makeAllPairs [] = []
makeAllPairs (x:xs) = pairs (x:xs) ++ makeAllPairs xs
    where pairs [] = []
          pairs (x:xs) = map (x,) (x:xs) -- a pair with itself is added too

reproductionSize :: Int -> Int
reproductionSize = ceiling . (\x ->   0.5 * (1 + sqrt (x * 8 + 1)) ) . fromIntegral 
-- reproductionSize = ceiling . (0.5 +) sqrt . (0.25 + ) . fromIntegral . (* 2)
-- 0.5 * (2 + sqrt (1 + 8*m))
-- reproductionSize = ceiling . sqrt . fromIntegral . (* 2)
-- m =~ n*(n-1)/2

signumT :: (Eq a,Num a, Num b) => a -> b
signumT x = case signum x of
        0 -> 0
        1 -> 1
        (-1) -> -1
















