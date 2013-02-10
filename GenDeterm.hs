{-# LANGUAGE TupleSections #-}

module GenDeterm
       (orderByFitness
       ,crossLinesBySchema
       ,crossLinesBySchemaSaving
       ,makeAllPairs
	   )  where

import Control.Applicative ( (<*>) )
import Data.Function  (on)
import Data.List (sortBy, sort, subsequences)

-- | the algo will MINIMIZE (!) the fitness function
--                     f

orderByFitness :: Ord b => (a -> b) -> [a] -> [a]
orderByFitness f = sortBy (compare `on` f) 


crossLinesBySchema, clbs :: [Int] -> [a] -> [a] -> [a]
crossLinesBySchema x p q = take n $ clbs x p q
    where n = length $ zip p q -- choose lowest
-- | does not always hold to the smallest array
clbs _ [] _ = []
clbs _ _ [] = []
clbs [] _ _ = []
clbs (x:xs) as bs = take x as ++
     clbs xs (drop x bs) (drop x as)

-- | this version saves the rest of elements from [Int] array:
--   randomness is precious in brutal pure functional world
crossLinesBySchemaSaving, clbss :: [Int] -> [a] -> [a] -> ([a],[Int])
crossLinesBySchemaSaving x p q = clbss x p q
   --  where n = length $ zip p q -- choose lowest
-- | does not always hold to the smallest array
clbss x [] _ = ([],x)
clbss x _ [] = ([],x)
-- this cannot happen: clbss [] _ _ = []
clbss (x:xs) as bs = (take x as ++ ans, rest)
   where (ans, rest) = clbss xs (drop x bs) (drop x as)

makeAllPairs :: [a] -> [(a,a)]
makeAllPairs [] = []
makeAllPairs (x:xs) = pairs (x:xs) ++ makeAllPairs xs
    where pairs [] = []
          pairs (x:xs) = map (x,) (x:xs) -- a pair with itself is added too


















