import System.Random
import Control.Applicative ( (<*>) )
import Data.Function  (on)
import Data.List (sortBy, sort)

genomeLength = length sourceData
sourceData = [1.2, -3.0, 0, -2, 4.1, 0, 0, 0, 0, 0]
destinationSum = 0
singleMutationProbability = 0.33 -- 1.0 / fromIntegral genomeLength
refGen = read "aaa" :: StdGen

type GenomeElem = Double

-- | the algo will MAXIMIZE the fitness function
fitness = negate . abs . (destinationSum - ) . sum . zipWith (*) sourceData

makeRandomGenome :: RandomGen g => g -> [GenomeElem]
makeRandomGenome = take genomeLength . randomRs (-1,1)

mutateOneNumber :: RandomGen g => g -> GenomeElem -> GenomeElem
mutateOneNumber g x = if doMutation then y else x
   where
    doMutation = singleMutationProbability > a 
    (a,g') = random g
    y = x * fst (randomR (-2,2) g')

-- | split is said to be not that good,
--   there must be a more effective solution to distribute randomness between functions
iterateRandomGen :: RandomGen g => g -> [g]
iterateRandomGen g = map fst $ iterate split' (g,g)
    where split' = split . snd

mutateGenome :: RandomGen g => g -> [GenomeElem] -> [GenomeElem]
mutateGenome g = mapWithRandom g mutateOneNumber

-- | returns INFINITE list !
applyRandom :: RandomGen g => g -> (g -> a) -> [a]
applyRandom g f = map f (iterateRandomGen g)

mapWithRandom :: RandomGen g => g -> (g -> a -> b) -> [a] -> [b]
mapWithRandom g f = zipWith ($) (applyRandom g f) 

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

randomGenomes :: RandomGen g => g -> [[GenomeElem]]
randomGenomes g = applyRandom g makeRandomGenome




























