module GenStats where

topRepeats :: Eq a => [a] -> Int
topRepeats [] = 0
topRepeats (x:xs) = length $ takeWhile (==x) xs 