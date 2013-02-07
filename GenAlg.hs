module GenAlg(
        mutateGenomeWith
       ,orderByFitness
       ,crossGenomes  -- narrow way out of the box
       ,crossLinesBySchema
	   )  where

import System.Random
import Control.Applicative ( (<*>) )
import Data.Function  (on)
import Data.List (sortBy, sort, subsequences)
import UsualRandom

singleMutationProbability = 0.33 -- 1.0 / fromIntegral genomeLength


type GenomeElem = Double

-- | the algo will MAXIMIZE the fitness function
--                 fitness

-- ! infinite!
makeRandomGenome :: RandomGen g => g -> [GenomeElem]
makeRandomGenome = randomRs (-1,1)
-- ! infinite!
randomGenomes :: RandomGen g => g -> [[GenomeElem]]
randomGenomes g = applyRandom g makeRandomGenome

mutateOneNumber :: RandomGen g => g -> GenomeElem -> GenomeElem
mutateOneNumber g x = if doMutation then y else x
   where
    doMutation = singleMutationProbability > a 
    (a,g') = random g
    y = x * fst (randomR (-2,2) g')



orderByFitness :: Ord b => (a -> b) -> [a] -> [a]
orderByFitness f = reverse . sortBy (compare `on` f) 

crossGenomes :: RandomGen g => g -> [a] -> [a] -> [a]
crossGenomes g xs ys =  take l $ crossLinesBySchema ks xs ys
	where 
        l = length $ zip xs ys
        ks = map raise $ randoms g
        k = (fromIntegral l :: Double) / 3
        raise = round . (k * )


-- | does not always hold to the smallest array
crossLinesBySchema :: [Int] -> [a] -> [a] -> [a]
crossLinesBySchema _ [] _ = []
crossLinesBySchema _ _ [] = []
crossLinesBySchema [] _ _ = []
crossLinesBySchema (x:xs) as bs = take x as ++
     crossLinesBySchema xs (drop x bs) (drop x as)



updateGeneration :: (RandomGen g,Ord k) => ([a]->k) -> g -> [[a]] -> [[a]]
updateGeneration fitness g as = makeNewPop popSize mutants chosen
    where popSize = length as
          chosenSize = max 1 $ round $ (fromIntegral popSize :: Double) * 0.1
          chosen = take chosenSize $ orderByFitness fitness as
          pairs = filter ((==2).length) $ subsequences chosen
          children = mapWithRandom g xxx $ pairs
          xxx g (x:y:_) = crossGenomes g x y
          mutants = children
          

makeNewPop popSize children parents = take popSize $ take k children ++ parents ++ drop k children 
   where k = min popSize (length children) `div` 3           

generations :: RandomGen g => ([a]->k) -> g -> [[a]] -> [ [[a]] ]
generations fitness g x = y:generations fitness g2 y
    where (g1,g2) = split g
          y = updateGeneration fitness g1 x






























