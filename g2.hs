import System.Random
import Control.Applicative ( (<$>) )
import Data.Function  (on)
import Data.List (sortBy, sort, subsequences)
import GenDeterm
import GenRand
import Control.Monad.Random

genomeLength = length $ zip sourceData1 sourceData2
sourceData1 = [1.2, -3.0, 0, -2, 4.1] -- , 0, 0, 0, 0, 0]
sourceData2 = take 10 $ [1.2, -3.0, 2, 2, 1.1] ++ repeat 0
destinationSum = 0

refGen = read "aaa" :: StdGen

type GenomeElem = Double

-- | the algo will MINIMIZE the fitness function
fitnessD s = abs . (destinationSum - ) . sum . zipWith (*) s 
fitness x =  fitnessD sourceData1 x + fitnessD sourceData2 x  

makeRandomGenome :: RS [GenomeElem]
makeRandomGenome = sequence $ (take genomeLength . repeat) $ getRandomR (-1,1)

initGenomes :: Int -> RS [[GenomeElem]]
initGenomes n = sequence $ (take n . repeat) makeRandomGenome






top5 pop = take 5 $ map fitness $ orderByFitness fitness pop
































