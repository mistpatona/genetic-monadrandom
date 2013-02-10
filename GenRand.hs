module GenRand
             (RS
             ,MutationParams(..)
             ,mutateWithRandP	
             ,mutateGenome
             ,mutateGenomes
             ,crossGenomes
             	) where 

import Control.Monad.Random
import Control.Applicative ( (<$>), liftA2 )
import System.Random

type RS = Rand StdGen

data MutationParams a = MutationParams 
     {   mutProbability :: Double
        ,mutRange :: (a,a)
        ,mutFunction :: a -> a -> a -- combine parameter with random value of the same type    
      } 


mutateWithRandP :: (Random a) => MutationParams a -> a -> RS a
mutateWithRandP p x = do
      r <- getRandom --  :: RS Double
      if r > mutProbability p 
          then return x 
      	  else mutFunction p x <$> getRandomR (mutRange p)

-- kinda usage example :  
-- evalRand (mutateWithRandP (MutationParams 0.5 (1,2) (*) ) (10::Float) ) (mkStdGen 92)

mapRand :: (a -> RS a) -> [a] -> RS [a]
mapRand = mapM
mutateGenome = mapRand

mutateGenomes f = mapM (mapM f)
-- example:
-- mutateGenomes (mutateWithRandP $ MutationParams 0.5 (-2,2) (*) ) [[1::Float,2..5],[3..10]] `evalRand` mkStdGen 92

makePairs :: [a] -> RS [(a,a)]
makePairs = undefined 

crossGenomes :: (Int,Int) -> [a] -> [a] -> RS [a]
crossGenomes _ [] j = return j
crossGenomes _ i [] = return i
crossGenomes bnds as bs =  do 
	    n <- getRandomR bnds
	    (take n as ++) <$> crossGenomes bnds (drop n bs) (drop n as)
-- example:
-- crossGenomes (1,4) "aaaaaaaaaaaaaaaaaaaaaaaaaa"  ['b'..'z'] `evalRand` mkStdGen 92


standardMulMutation = MutationParams 0.01 (-2,2) (*)
lightMulMutation = standardMulMutation { mutProbability = 0.001 }
-- moreLightMutation =   MutationParams 0.001 (-1,1)  (liftA2 (+) (*1.5) ((*0.5).signumT))
  -- multiply by koef (+/-)[0.5  to  2] or just sometimes 0


signumT :: (Eq a,Num a, Num b) => a -> b
signumT x = case signum x of
        0 -> 0
        1 -> 1
        (-1) -> -1

