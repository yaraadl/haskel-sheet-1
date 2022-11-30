--yara adel hassan mohamed 
--19100683
--7th assignmnet spl
---------------------------------------------
--final answer for q1
--q1
localMax :: [Int] -> [Int]
localMax []       = []
localMax (x:[])   = []
localMax (x:y:[]) = []
localMax (x:y:z:zs)
    | y > z && y > x = y:localMax (y:z:zs)
    | otherwise = localMax (y:z:zs)
------------------------------------------------
-- final answer
--Q2
hist :: Eq a => [a] -> [(a, Int)]
removeDup :: Eq a => [a] -> [a]

removeDup (x:[]) = []
removeDup (x:y:xs) = if x == y 
                     then removeDup(x:xs)
                     else y:removeDup(x:xs)
hist [] = []
hist (x:xs) = zip [x] [(length $ filter (==x) (x:xs))]  ++ hist(removeDup (x:xs) )
----------------------------
-----------------------------------------------
--final answer for q3
--q3
collatz :: Integer -> Integer
collatz 1 = 0 
collatz x = if x`mod` 2 == 0 
            then 1+collatz (x`div`2)
            else 1+collatz y 
            where y = 3 *x +1
-------------------------------------------------------
--final answer for q4
--Q4
hamming :: Eq a => [a] -> [a] -> Int
hamming la lb = length (filter id (zipWith (/=) la lb))
----------------------------
--final answer for q5
--q5
smaller :: Ord b => [b] ->[b] -> [b]
smaller [] ys = [] 
smaller xs [] = []
smaller (x:xs)(y:ys) = if(x <= y)
                       then x : smaller  xs ys
                       else y : smaller  xs ys
------------------------------

