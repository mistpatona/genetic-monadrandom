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
updateGeneration fitness g as = updateGenerationNeck neckSize fitness g as
    where popSize = length as
          neckSize = max 1 $ round $ (fromIntegral popSize :: Double) * 0.1
          
 
updateGenerationNeck :: (RandomGen g,Ord k) => Int -> ([a]->k) -> g -> [[a]] -> [[a]]
updateGenerationNeck chosenSize fitness g as = take popSize mutants
    where popSize = length as
          chosen = take chosenSize $ orderByFitness fitness as
          pairs = filter ((==2).length) $ subsequences chosen -- here is a big simplification
          children = mapWithRandom g xxx $ pairs
          xxx g (x:y:_) = crossGenomes g x y
          mutants = children          
      

generations :: RandomGen g => (g -> b -> b) -> g -> b -> [b]
generations f g x = y:generations' f g2 y
    where (g1,g2) = split g
          y = f g1 x



























