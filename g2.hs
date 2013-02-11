import System.Random
import Control.Applicative ( (<$>) )
import Data.Function  (on)
import Data.List (sortBy, sort, subsequences, nub)
import GenDeterm
import GenRand
import Control.Monad.Random
import Control.Monad ( (>=>) )


genomeLength = 5
sourceMatrix =[[1.2,-3,  2,   2, 1.1]
               ,[1, -3,  0,  -2, 4.1]
               ,[0,  2,  -1,  3, 4]
               ,[3,  3,  2,   2, 1]
               ,[1,  4,  2,   0,-1]]
destinationGenome = [1,2,3,4,5]

matrixMul :: Num a => [[a]] -> [a] -> [a]
matrixMul as bs =  map (sum . zipWith (*) bs) as
matrixB = map negate $ matrixMul sourceMatrix destinationGenome
fitness x = sum $ map abs $ zipWith (+) (matrixMul sourceMatrix x) matrixB
-- | the algo will MINIMIZE the fitness function

type GenomeElem = Double
makeRandomGenome :: RS [GenomeElem]
makeRandomGenome = sequence $ replicate genomeLength  $ getRandomR (-1,1)
initGenomes :: Int -> RS [[GenomeElem]]
initGenomes n = sequence $ replicate n makeRandomGenome

takeForBreed n = take (reproductionSize n)  

pairsForBreed = take m . makeAllPairs . takeForBreed m . cycle
    where m = genomeLength

-- | f is fitness function
makePairsForBreed f = pairsForBreed . orderByFitness f 

crossAll :: (Int,Int) -> [([a],[a])] -> RS [[a]] 
crossAll chunkSizeBounds = mapM (uncurry $ crossGenomes chunkSizeBounds)

-- mutateAll = mutateGenomes (mutateWithRandP $ MutationParams 0.05 (-1.01::GenomeElem,1.1) (*) )
mutateAll :: [[GenomeElem]] -> RS [[GenomeElem]]
mutateAll = mutateGenomes (mutateP mutateA 0.05)


mutateP :: (a->RS a) -> Double -> a -> RS a
mutateP f p x = do p1 <- getRandom
                   if p1<p then f x else return x 

mutateA :: (Random a,Floating a) => a -> RS a
mutateA x = do r1 <- getRandomR (-1::Float,1)
               r2 <- getRandomR (-1,1)
               return $ x * signumT r1 * exp r2

crossAndMutate bnds = crossAll bnds >=> mutateAll

makeNewGen = crossAndMutate (1,2) . makePairsForBreed fitness

bindN :: (Monad m) => Int -> (a -> m a) -> (a -> m a) 
bindN 0 f = f
bindN n f = f >=> bindN (n-1) f 
passGens n = bindN n makeNewGen

top5 pop = take 5 $ map fitness $ orderByFitness fitness pop
































