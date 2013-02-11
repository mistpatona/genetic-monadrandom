{-# LANGUAGE TupleSections #-}

module GenDeterm
       (orderByFitness
       ,makeAllPairs
       ,reproductionSizeAllPairs
       ,makeMisalliancePairs
       ,replicateTopsExp
       ,magnifyEachByCoef
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

-- | input must be in descenting quality order
--   there is enough TODO to optimize in this design
--  in usual use,  "tops" must be included in "alls" 
makeMisalliancePairs :: [a] -> [a] -> [(a,a)]
makeMisalliancePairs _ [] = []
makeMisalliancePairs [] _ = []
makeMisalliancePairs (top:tops) alls = map (top,) alls ++ makeMisalliancePairs tops (tailDrop k alls)
    where tailDrop n = reverse . drop n . reverse 
          k = length alls `div` length tops


-- | Exponentially (**) descending from k to 1 to the end of the list
replicateTopsExp :: Double -> [a] -> [a]
replicateTopsExp k = magnifyEachByCoef (topsMagnificationCoef k)

magnifyEachByCoef :: (Int -> Int -> Int) -> [a] -> [a]
magnifyEachByCoef f xs = mec (f $ length xs) xs 1

mec _ [] _ = []
mec f (x:xs) n = replicate (f n) x ++ mec f xs (n+1)
    
topsMagnificationCoef :: Double -> Int -> Int -> Int
topsMagnificationCoef k _    1 = round k
topsMagnificationCoef k size n = round x
    where x = (sz / fromIntegral n) ** (log k / log sz)
          sz = fromIntegral size

reproductionSizeAllPairs :: Int -> Int
reproductionSizeAllPairs = ceiling . (\x ->   0.5 * (1 + sqrt (x * 8 + 1)) ) . fromIntegral 
-- reproductionSize = ceiling . (0.5 +) sqrt . (0.25 + ) . fromIntegral . (* 2)
-- 0.5 * (2 + sqrt (1 + 8*m))
-- reproductionSize = ceiling . sqrt . fromIntegral . (* 2)
-- m =~ n*(n-1)/2

signumT :: (Eq a,Num a, Num b) => a -> b
signumT x = case signum x of
        0 -> 0
        1 -> 1
        (-1) -> -1
















