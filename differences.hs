-- Given a set of positive integers, partition the set into two such that the difference between the sum of the two sets is minimized.

import Data.List
  (sort
  ,sortOn
  ,delete)

type Partition = ([Int],[Int])
type Difference = (Int,Int,Int)

mkDifference :: Int -> Int -> Difference
mkDifference a b =
  (a-b,a,b)

differences :: [Int] -> [Difference]
differences =
  zipWith mkDifference <*> tail

remove :: Difference -> [Int] -> [Int]
remove (_,a,b) =
  delete a . delete b

use :: Difference -> Partition -> Partition
use (_,a,b) (as,bs) =
  (a:as,b:bs)
  
difference :: Difference -> Int
difference (d,_,_) =
  d
  
evaluate :: Partition -> Int
evaluate (as,bs) =
  sum as - sum bs

-- greedy, suboptimal, two sorts, gross
partition :: [Int] -> Partition -> Partition
partition [] p =
  p
partition [a] (as,bs) =
  (a:as,bs)
partition s p =
  partition (remove next s) (use next p)
  where
    next = head . reverse . sortOn difference . differences . sort $ s
  
main =
  sequence [print p, print $ evaluate p]
  where
    p = partition [2,10,3,8,5,7,9,5,3,2,3] ([],[])
