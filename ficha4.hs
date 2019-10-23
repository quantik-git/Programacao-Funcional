-- Exercicio 1
--a,b [x | x <- [1..20], mod x 6 == 0]
--c [(x, 30-x) | x <- [10..20]]
--d [sum $ filter odd [1..x] | x <- [1..10]]

-- Exercicio 2
-- a) [2^x | x <- [1..10]]
-- b) [(x,y) | x <- [1..5], y <- [1..10], x+y == 6]
-- c) [[1..x] | x <- [1..5]]
-- d) [replicate x 1 | x <- [1..5]]
-- e) [product [1..x] | x <- [1..6]] 

-- Exercicio 3
import Data.Char  (isDigit, isAlpha)

digitAlpha :: String -> (String,String)
digitAlpha = foldr (\x (a,b) -> if (isDigit x)
    then ((x:a), b)
    else if (isAlpha x)
      then (a, (x:b))
      else (a, b)) ("","")

-- Exercicio 4
nzp :: [Int] -> (Int,Int,Int)
nzp = foldr (\x (a,b,c) -> if x < 0
    then (a+1,b,c)
    else if x == 0
        then (a,b+1,c)
        else (a,b,c+1)) (0,0,0)

-- Exercicio 5
divMod' :: Integral a => a -> a -> (a, a)
divMod' dividendo divisor = aux 0 dividendo divisor
    where aux acc dividendo divisor | dividendo >= divisor = aux (acc+1) (dividendo-divisor) divisor
                                    | otherwise = (acc, dividendo)

-- Exercicio 6
fromDigits :: [Int] -> Int
fromDigits = foldl (\acc x -> x + 10 * acc ) 0

-- Exercicio 7
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit (x:xs) = if x > 0 then aux x x xs else aux 0 x xs 
  where
    aux maximo _ [] = maximo
    aux maximo acc (y:ys) = if acc+y > maximo
      then aux (acc+y) (acc+y) ys
      else aux maximo (acc+y) ys

-- Exercicio 8
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fibo 0 1 2 n
  where
    fibo acc1 acc2 n alvo | n == alvo = acc1 + acc2
                          | otherwise = fibo acc2 (acc1 + acc2) (n + 1) alvo
