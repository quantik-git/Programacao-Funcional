-- Exercicio 3
import Data.Char

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (x:xs) = if elem x ['0'..'9'] then x : soDigitos xs else soDigitos xs

minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (x:xs) = if elem x ['a'..'z'] then 1 + minusculas xs else minusculas xs

nums :: String -> [Int]
nums [] = []
nums (x:xs) = if elem x ['0'..'9'] then (ord x - ord '0') : nums xs else nums xs

-- Exercicio 4
type Monomio = (Float, Int)
type Polinomio = [Monomio]

-- pol = [(2,3), (3,4), (5,3), (4,5)]

conta :: Int -> Polinomio -> Int
conta n lista = length [ y | (x, y) <- lista, y == n ]

grau :: Polinomio -> Int
grau lista = maximum [ b | (a, b) <- lista ]

selgrau :: Int -> Polinomio -> Polinomio
selgrau n lista = [ (x, y) | (x, y) <- lista, y == n ]

calcula :: Float -> Polinomio -> Float
calcula n lista = sum [ a*(n^b) | (a, b) <- lista]

simp :: Polinomio -> Polinomio
simp lista = [ (a, b) | (a, b) <- lista, a == 0 ]

mult :: Monomio -> Polinomio -> Polinomio
mult (x, y) lista = [ (x*a, y*b) | (a, b) <- lista]

-- auxiliares
somaMonomio ((a, b):[]) = (a, b)
somaMonomio ((a, b):(c, _):xs) = somaMonomio ((a+c, b):xs)

graus [] = []
graus (x:xs) = if elem x xs then graus xs else x : graus xs

normaliza :: Polinomio -> Polinomio
normaliza lista = [ somaMonomio (selgrau x lista) | x <- aux]
  where aux = graus [ b | (a, b) <- lista]

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = normaliza $ concat [ mult (a, b) p2 | (a, b) <- p1]

-- auxiliares
quicksort1 [] = []
quicksort1 (x@(a, b):xs) =
  let smallerSorted = quicksort1 [(c, d) | (c, d) <- xs, c <= a]
      biggerSorted  = quicksort1 [(c, d) | (c, d) <- xs, c >  a]
  in  smallerSorted ++ [x] ++ biggerSorted

quicksort' [] = []
quicksort' (x:xs) =
  let smallerSorted = quicksort' [a | a <- xs, a <= x]
      biggerSorted = quicksort' [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

ordena :: Polinomio -> Polinomio
ordena lista = concat [quicksort1 $ selgrau n lista | n <- aux]
  where aux = quicksort' $ graus [ b | (a, b) <- lista]

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena p1 == ordena p2