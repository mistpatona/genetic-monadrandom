import System.Random
import Control.Applicative ( (<$>) )
import Data.Function  (on)
import Data.List (sortBy, sort, subsequences, nub)
import GenDeterm
import GenRand
import Control.Monad.Random
import Control.Monad ( (>=>), liftM2 )


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
makeRandomGenome = sequence $ replicate genomeLength $ getRandomR (-1,1)
initGenomes :: Int -> RS [[GenomeElem]]
initGenomes n = sequence $ replicate n makeRandomGenome

takeForBreed n = take (reproductionSizeAllPairs n)  

pairsForBreedOld = take m . makeAllPairs . takeForBreed m . cycle
    where m = genomeLength

pairsForBreed inp = (take m . cycle . misalliancePairs . takeForBreed (m `div` 2) ) inp 
    where m = length inp

misalliancePairs xs = makeMisalliancePairs  (replicateTopsExp 10.0 tops) xs
     where tops = take (length xs `div` 4) xs

-- | f is fitness function
makePairsForBreed f = pairsForBreed . orderByFitness f 

crossAll :: (Int,Int) -> [([a],[a])] -> RS [[a]] 
crossAll chunkSizeBounds = mapM (uncurry $ crossGenomes chunkSizeBounds)

mutateAll :: [[GenomeElem]] -> RS [[GenomeElem]]
mutateAll = mutateGenomes (mutateP mutateR 0.02 >=> mutateP mutateA 0.05)

mutateP :: (a->RS a) -> Double -> a -> RS a
mutateP f p x = do p1 <- getRandom
                   if p1<p then f x else return x 

mutateA :: (Random a,Floating a) => a -> RS a
mutateA x = do r1 <- getRandomR (-1::Float,1)
               r2 <- getRandomR (-1,1)
               return $ x * signumT r1 * exp r2

mutateR :: (Random a, Num a) => a -> RS a
mutateR _ = getRandomR (-2,2)

crossAndMutate bnds = crossAll bnds >=> mutateAll

makeNewGen = crossAndMutate (1,2) . makePairsForBreed fitness

bindN :: (Monad m) => Int -> (a -> m a) -> (a -> m a) 
bindN 0 _ = return
bindN n f = f >=> bindN (n-1) f 

passGens n = bindN n makeNewGen

-- | gives an infinite answer!  
iterateM :: (Monad m, Functor m) => (a->m a) -> a -> m [a]
--iterateM f = f >>= (\y -> (y:) <$> iterateM f y )
iterateM f x = do y <- f x
                  (y:) <$> iterateM f y



top5 pop = take 5 $ map fitness $ orderByFitness fitness pop
































