-- 1
-- Por exemplo, enumFromTo 1 5 corresponde `a lista [1,2,3,4,5]
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' start stop
    | start <= stop = start : enumFromTo' (start+1) stop
    | otherwise = []


-- 2
-- Por exemplo, enumFromThenTo 1 3 10 corresponde `a lista [1,3,5,7,9].
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' start inc stop
    | start <= stop = start : enumFromThenTo' (start+inc-1) inc stop
    | otherwise = []


-- 3
-- Por exemplo, (++) [1,2,3] [10,20,30] corresponde `a lista [1,2,3,10,20,30]
(++!) :: [a] -> [a] -> [a]
(++!) [] list = list
(++!) (h:t) list = h : (t ++! list)


-- 4
-- Por exemplo, (!!) [10,20,30] 1 corresponde a 20.
(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) n = t !!! (n-1)


-- 5
-- Por exemplo, reverse [10,20,30] corresponde a [30,20,10].
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' t ++ [h]


-- 6
-- Por exemplo, take 2 [10,20,30] corresponde a [10,20].
take' :: Int -> [a] -> [a]
take' 0 list = []
take' n (h:t) = h : take' (n-1) t


-- 7
-- Por exemplo, drop 2 [10,20,30] corresponde a [30].
drop' :: Int -> [a] -> [a]
drop' 0 list = list
drop' n (h:t) = drop' (n-1) t


-- 8
-- Por exemplo, zip [1,2,3] [10,20,30,40] corresponde a [(1,10),(2,20),(3,30)].
zip' :: [a] -> [b] -> [(a,b)]
zip' [] ls = []
zip' ls [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


-- 9
-- Por exemplo, replicate 3 10 corresponde a [10,10,10].
replicate' :: Int -> a -> [a]
replicate' 0 num = []
replicate' n num = num : replicate' (n-1) num


-- 10
-- Por exemplo, intersperce 1 [10,20,30] corresponde a [10,1,20,1,30].
intersperse' :: a -> [a] -> [a]
intersperse' n [x] = [x]
intersperse' n (h:t) = h : n : intersperse' n t


-- 11
-- Por exemplo, group [1,2,2,3,4,4,4,5,4] corresponde a [[1],[2,2],[3],[4,4,4],[5],[4]].
group' :: Eq a => [a] -> [[a]]
group' (h:t) = let (grouped, rest) = group'aux [h] t
    in if rest == [] 
        then [grouped]
        else grouped : group' rest

group'aux (x:xs) [] = ((x:xs), [])
group'aux (x:xs) (y:ys)
    | x == y = group'aux (y:x:xs) ys
    | otherwise = ((x:xs), (y:ys))


-- 12
-- Por exemplo, concat [[1],[2,2],[3],[4,4,4],[5],[4]] corresponde a [1,2,2,3,4,4,4,5,4].
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t


-- 13
-- Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]].
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' ls = inits' (init ls) ++ [ls]


-- 14
-- Por exemplo, tails [1,2,3] corresponde a [[1,2,3],[2,3],[3],[]].
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' (h:t) = [(h:t)] ++ tails' t


-- 15
-- Por exemplo, heads [[2,3,4],[1,7],[],[8,5,3]] corresponde a [2,1,8].
heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' (h:t) = head h : heads' t

-- heads ls = [head l | l <- ls, not (null l)]


-- 16
-- Por exemplo, total [[2,3,4],[1,7],[],[8,5,3]] corresponde a 8.
total :: [[a]] -> Int
total [] = 0
total (h:t) = comp h + total t

comp [] = 0
comp (h:t) = 1 + comp t


-- 17
-- Por exemplo, fun [("rui",3,2), ("maria",5,2), ("ana",43,7)] corresponde a [("rui",2), ("maria",2), ("ana",7)]
fun' :: [(a,b,c)] -> [(a,c)]
fun' [] = []
fun' ((a,b,c):t) = (a,c) : fun' t


-- 18
-- Por exemplo, cola [("rui",3,2), ("maria",5,2), ("ana",43,7)] corresponde a "ruimariaana".
cola :: [(String,b,c)] -> String 
cola [] = ""
cola ((a,b,c):t) = a ++ cola t


-- 19
-- Por exemplo, idade 2021 26 [("rui",1995), ("maria",2009), ("ana",1947)] corresponde a ["rui","ana"].
idade :: Int -> Int -> [(String,Int)] -> [String]
idade ano max list = [a | (a, b) <- list, (ano-b) <= max]


-- 20
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n 0 = []
powerEnumFrom' n m = powerEnumFrom' n (m-1) ++ [n^(m-1)]


-- 21
-- 2 ≤ m ≤ √n e mod n m = 0
isPrime' :: Int -> Bool
isPrime' n = isPrime'aux n 2

isPrime'aux :: Int -> Int -> Bool
isPrime'aux n m
    | fromIntegral m <= sqrt (fromIntegral n) = if mod n m == 0 then False else isPrime'aux n (m+1)
    | otherwise = True


-- 22
-- Por exemplo, isPrefixOf [10,20] [10,20,30] corresponde a True enquanto que isPrefixOf [10,30] [10,20,30] corresponde a False.
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] ls = True
isPrefixOf' ls [] = False
isPrefixOf' (x:xs) (y:ys)
    | x == y = isPrefixOf' xs ys
    | otherwise = False


-- 23
-- Por exemplo, isSuffixOf [20,30] [10,20,30] corresponde a True enquanto que isSuffixOf [10,30] [10,20,30] corresponde a False.
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] [] = True
isSuffixOf' ls [] = False
isSuffixOf' (x:xs) (y:ys)
    | (length xs) == (length ys) = if x == y then isSuffixOf' xs ys else False
    | otherwise = isSuffixOf' (x:xs) ys

-- isSuffixOf' xs ys = isPrefixOf (reverse xs) (reverse ys)


-- 24
-- Por exemplo, isSubsequenceOf [20,40] [10,20,30,40] corresponde a True enquanto que isSubsequenceOf [40,20] [10,20,30,40] corresponde a False.
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] ls = True
isSubsequenceOf' ls [] = False
isSubsequenceOf' (x:xs) (y:ys)
    | x == y = isSubsequenceOf' xs ys
    | otherwise = isSubsequenceOf' (x:xs) ys


-- 25
-- Por exemplo, elemIndices 3 [1,2,3,4,3,2,3,4,5] corresponde a [2,4,6].
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' n list = elemIndices'aux n 0 list

elemIndices'aux n idx [] = []
elemIndices'aux n idx (h:t)
    | n == h = idx : elemIndices'aux n (idx+1) t
    | otherwise = elemIndices'aux n (idx+1) t


-- 26
-- Por exemplo, nub [1,2,1,2,3,1,2] corresponde a [1,2,3].
nub' :: Eq a => [a] -> [a]
nub' list = nub'aux [] list

nub'aux acc [] = acc
nub'aux acc (h:t)
    | elem h acc = nub'aux acc t
    | otherwise = nub'aux (acc ++ [h]) t


-- 27
-- Por exemplo, delete 2 [1,2,1,2,3,1,2] corresponde a [1,1,2,3,1,2]
delete' :: Eq a => a -> [a] -> [a]
delete' n [] = []
delete' n (h:t)
    | n == h = t
    | otherwise = h : delete' n t


-- 28
-- Por exemplo, (\\)[1,2,3,4,5,1] [1,5] corresponde a [2,3,4,1].
(\\!) :: Eq a => [a] -> [a] -> [a]
(\\!) [] ys = []
(\\!) xs [] = xs
(\\!) (x:xs) ys
    | elem x ys = (\\!) xs (delete' x ys)
    | otherwise = x : (\\!) xs (delete' x ys)

-- (\\!) xs [] = xs
-- (\\!) xs (y:ys) = (\\!) (delete' y xs) ys


-- 29
-- Por exemplo, union [1,1,2,3,4] [1,5] corresponde a [1,1,2,3,4,5].
union' :: Eq a => [a] -> [a] -> [a]
union' [] ys = ys
union' (x:xs) ys = x : union' xs (union'aux x ys)

union'aux n [] = []
union'aux n (h:t)
    | n == h = union'aux n t
    | otherwise = h : union'aux n t


-- 30
-- Por exemplo, intersect [1,1,2,3,4] [1,3,5] corresponde a [1,1,3].
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] ys = []
intersect' (x:xs) ys
    | elem x ys = x : intersect' xs ys
    | otherwise = intersect' xs ys

-- intersect' xs ys = [x | x <- xs, elem x ys]


-- 31
-- Por exemplo, insert 25 [1,20,30,40] corresponde a [1,20,25,30,40].
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (h:t)
    | n > h = h : insert' n t
    | otherwise = h : n : t


-- 32
-- Por exemplo, unwords ["Programacao", "Funcional"] corresponde a "Programacao Funcional".
unwords' :: [String] -> String
unwords' [] = ""
unwords' [n] = n
unwords' (h:t) = h ++ " " ++ unwords' t


-- 33
-- Por exemplo, unlines ["Prog", "Func"] corresponde a "Prog\nFunc\n".
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t


-- 34
pMaior :: Ord a => [a] -> Int
pMaior (h:t) = let len = length t
    in len - pMaior'aux h len t

pMaior'aux n idx [] = idx
pMaior'aux n idx (h:t)
    | n < h = pMaior'aux h (length t) t
    | otherwise = pMaior'aux n idx t


-- 35
-- Por exemplo, lookup ’a’ [(’a’,1),(’b’,4),(’c’,5)] corresponde `a lista Just 1.
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' n [] = Nothing
lookup' n ((a,b):t)
    | n == a = Just b
    | otherwise = lookup' n t


-- 36
-- Por exemplo, preCrescente [3,7,9,6,10,22] corresponde a [3,7,9].
preCrescente :: Ord a => [a] -> [a]
preCrescente list = preCrescente'aux [] list

preCrescente'aux acc [] = acc
preCrescente'aux acc list = let (pre, rest) = preCrescente'aux1 [] list
    in if (length pre) > (length acc)
        then preCrescente'aux pre rest
        else preCrescente'aux acc rest

preCrescente'aux1 :: Ord a => [a] -> [a] -> ([a], [a])
preCrescente'aux1 acc [n] = (acc ++ [n], [])
preCrescente'aux1 acc (f:s:t)
    | f <= s = preCrescente'aux1 (acc ++ [f]) (s:t)
    | otherwise = ((acc ++ [f]), (s:t))


-- 37
-- Assuma insert :: Ord a => a -> [a] -> [a]
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)


-- 38
-- Por exemplo, menor "sai" "saiu" corresponde a True enquanto que menor "programacao" "funcional" corresponde a False.
menor :: String -> String -> Bool
menor [] ys = True
menor xs [] = False
menor (x:xs) (y:ys)
    | x <= y = menor xs ys
    | otherwise = False


-- 39
-- Por exemplo, elemMSet ’a’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a True enquanto que elemMSet ’d’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a False.
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet n [] = False
elemMSet n ((a,b):t)
    | n == a = True
    | otherwise = elemMSet n t


-- 40
-- Por exemplo, converteMSet [(’b’,2), (’a’,4), (’c’,1)] corresponde a "bbaaaac".
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet (h:t) = (converteMSet'aux h) ++ converteMSet t

converteMSet'aux (a,0) = []
converteMSet'aux (a,b) = a : converteMSet'aux (a,(b-1))


-- 41
-- Por exemplo, insereMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),(’a’,4), (’c’,2)].
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((a,b):t)
    | n == a = (a,(b+1)) : t
    | otherwise = (a,b) : insereMSet n t


-- 42
-- Por exemplo, removeMSet ’c’ [(’b’,2), (’a’,4), (’c’,1)] corresponde a [(’b’,2),(’a’,4)].
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet n [] = []
removeMSet n ((a,b):t)
    | n == a = if (b-1) == 0 then t else (a,(b-1)) : t
    | otherwise = (a,b) : removeMSet n t


-- 43
-- Por exemplo, constroiMSet "aaabccc" corresponde a [(’a’,3), (’b’,1), (’c’,3)].
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:t) = let (tuple,rest) = constroiMSet'aux (h,1) t
    in tuple : constroiMSet rest

constroiMSet'aux acc [] = (acc, [])
constroiMSet'aux (a,b) (h:t)
    | a == h = constroiMSet'aux (a,b+1) t
    | otherwise = ((a,b), (h:t))


-- 44
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([], [])
partitionEithers' ((Left a):t) = let (l, r) = partitionEithers' t
    in (a:l, r)
partitionEithers' ((Right b):t) = let (l, r) = partitionEithers' t
    in (l, b:r)


-- 45
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just a):t) = a : catMaybes' t
catMaybes' ((Nothing):t) = catMaybes' t


-- 46
data Movimento = Norte | Sul | Este | Oeste
    deriving Show
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x1,y1) (x2,y2)
    | x1 < x2 = Este : caminho (x1+1,y1) (x2,y2)
    | x1 > x2 = Oeste : caminho (x1-1,y1) (x2,y2)
    | y1 < y2 = Norte : caminho (x1,y1+1) (x2,y2)
    | y1 > y2 = Sul : caminho (x1,y1-1) (x2,y2)
    | otherwise = []


-- 47
-- Por exemplo hasLoops (1,2) [Norte,Sul,Este] corresponde a True
hasLoops :: (Int,Int) -> [Movimento] -> Bool
hasLoops (x1,y1) [] = False
hasLoops (x1,y1) list = hasLoops'aux (0,0) list

move :: (Int, Int) -> Movimento -> (Int, Int)
move (x,y) Norte = (x+1,y)
move (x,y) Sul = (x-1,y)
move (x,y) Este = (x,y+1)
move (x,y) Oeste = (x,y-1)

hasLoops'aux coord [] = False
hasLoops'aux coord (h:t) = let (x,y) = move coord h
    in if x == 0 && y == 0
        then True
        else hasLoops'aux (x,y) t


-- 48
-- Por exemplo contaQuadrados [(Rect (0.0,0.0) (1.0,1.0))] corresponde a 1
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x1,y1) (x2,y2)):t)
    | (abs (abs x1 - abs x2)) == (abs (abs y1 - abs y2)) = 1 + contaQuadrados t
    | otherwise = contaQuadrados t


--49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (h:t) = areaTotal'aux h + areaTotal t

areaTotal'aux (Rect (x1,y1) (x2,y2)) = (abs (abs x1 - abs x2)) * (abs (abs y1 - abs y2))


-- 50
data Equipamento = Bom | Razoavel | Avariado
    deriving Show
naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar ((Avariado):t) = naoReparar t
naoReparar (h:t) = 1 + naoReparar t