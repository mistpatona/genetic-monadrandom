module GenDeterm(
       ,orderByFitness
       ,crossLinesBySchema
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




















