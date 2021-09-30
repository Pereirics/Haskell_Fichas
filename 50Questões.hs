import Data.Char
--1.
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b | a > b = []
                | otherwise = a : enumFromTo' (a+1) b
--2.
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' m s n | (m <= s) && (s <= n) = m : enumFromThenTo' s (2*s - m) n
                      | (m >= s) && (s >= n) = m : enumFromThenTo' s (2*s - m) n
                      | otherwise = [m]
--3.
concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena l [] = l
concatena [] l = l
concatena (h:t) (x:xs) = h : concatena t (x:xs)
--4.
poselem :: [a] -> Int -> a
poselem [] _ = error "Lista Vazia!"
poselem (h:t) x | (x == 0) = h
              | otherwise = poselem t (x-1)
--5.
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:[]) = [h]
reverse' (h:t) = last t : reverse' (h:(init t))
--6.
take' :: Int -> [a] -> [a]
take' _ [] = []
take' x (h:t) | (x == 0) = []
              | otherwise = h : take' (x-1) t
--7.
drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' x (h:t) | (x == 0) = (h:t)
              | otherwise = drop' (x-1) t
--8.
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (x:xs) = (h,x) : zip' t xs
--9.
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (h:t) = if (x == h) then True else elem' x t
--10.
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x n = n : replicate (x-1) n
--11.
intersperce :: a -> [a] -> [a]
intersperce _ [] = []
intersperce x (h:[]) = [h]
intersperce x (h:t) = h : x : intersperce x t
--12.
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = grou [h] h t
  where
    grou hlista h [] = [hlista]
    grou hlista h (x:xs) | h == x = grou (hlista ++ [x]) x xs
                         | otherwise = hlista : grou [x] x xs
--13.
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t
--14.
inits' :: [a] -> [[a]]
inits' [] = []
inits' l = ini [] [] l
  where
    ini lista acc [] = [acc]
    ini lista acc (h:t) = lista : ini (acc ++ [h]) (acc ++ [h]) t
--15.
tails' :: [a] -> [[a]]
tails' [] = []
tails' l = tai l l l
  where
    tai lista acc [] = [acc]
    tai lista acc (h:t) = lista : tai t t t
--16.
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (h:t) (x:xs) | (h == x) = isPrefixOf t xs
                        | otherwise = False
--17.
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf [] _ = True
isSuffixOf _ [] = False
isSuffixOf l1 l2 | (last l1 == last l2) = isSuffixOf (init l1) (init l2)
                 | otherwise = False
--18.
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf _ [] = False
isSubsequenceOf [] _ = True
isSubsequenceOf (h:t) (x:xs) | (h == x) = isSubsequenceOf t xs
                             | otherwise = isSubsequenceOf (h:t) xs
--19.
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n (h:t) = aux n (h:t) 0
  where
    aux n [] acc = []
    aux n (h:t) acc | (n == h) = (acc) : aux n t (acc+1)
                    | otherwise = aux n t (acc+1)
--20.
nub :: Eq a => [a] -> [a]
nub l = aux l []
  where
    aux [] l = []
    aux (h:t) l | elem' h l = aux t l
                | otherwise = h : aux t (l ++ [h])
--21.
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (h:t) | (x == h) = t
               | otherwise = h : delete x t
--22.
remove' :: Eq a => [a] -> [a] -> [a]
remove' [] _ = []
remove' l [] = l
remove' (h:t) (x:xs) | elem' h (x:xs) = remove' t (aux h (x:xs))
                     | otherwise = h : remove' t (x:xs)
  where
    aux h []  = []
    aux h (x:xs) | (h == x) = xs
                 | otherwise = x : aux h xs
--23.
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l
union' l [] = l
union' (h:t) (x:xs) | elem x (h:t) = union' (h:t) xs
                    | otherwise = union' (h:t++[x]) xs
--24.
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' (h:t) (x:xs) | elem' h (x:xs) = h : intersect' t (x:xs)
                        | otherwise = intersect' t (x:xs)
--25.
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (h:t) | (a <= h) = a:h:t
                | otherwise = h : insert' a t
--26.
unwords' :: [String] -> String
unwords' [] = []
unwords' (h:[]) = h
unwords' (h:t) = h ++ " " ++ unwords' t
--27.
unlines' :: [String] -> String
unlines' [] = []
unlines' (h:t) = h ++ "\n" ++ unlines' t
--28.
pMaior' :: Ord a => [a] -> Int
pMaior' (h:t) = aux h (h:t) 0 0
  where
    aux maior [] pos posM = posM
    aux maior (h:t) pos posM | h > maior = aux h t (pos + 1) pos
                             | otherwise = aux maior t (pos + 1) posM
--29.
temRepetidos :: Eq a => [a] -> Bool
temRepetidos [] = False
temRepetidos (h:t) | elem h t = True
                   | otherwise = temRepetidos t
--30.
algarismos :: [Char] -> [Char]
algarismos [] = []
algarismos (h:t) | 48 <= ord h && ord h <= 57 = h : algarismos t
                 | otherwise = algarismos t
--31.
posImpares :: [a] -> [a]
posImpares (h:t) = aux (h:t) [] 0
  where
    aux [] l _ = l
    aux (h:t) l pos | mod pos 2 /= 0 = aux t (l ++ [h]) (pos+1)
                    | otherwise = aux t l (pos+1)
--32.
posPares :: [a] -> [a]
posPares (h:t) = aux (h:t) [] 0
  where
    aux [] l _ = l
    aux (h:t) l pos | mod pos 2 == 0 = aux t (l ++ [h]) (pos+1)
                    | otherwise = aux t l (pos+1)
--33.
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [h] = True
isSorted (h:t) | h <= (head t) = isSorted t
               | otherwise = False
--34.
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = insert' h (iSort t)
--35.
menor :: String -> String -> Bool
menor _ [] = False
menor [] _ = True
menor [h] [x] = if h<x then True else False
menor (h:t) (x:xs) | h <= x = menor t xs
                   | otherwise = False
--36.
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet x [] = False
elemMSet x ((a,_):t) | (x == a) = True
                     | otherwise = elemMSet x t
--37.
lengthMSet :: [(a,Int)] -> Int
lengthMSet [] = 0
lengthMSet ((_,n):t) = n + lengthMSet t
--38.
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,n):t) | (n /= 0) = a : converteMSet ((a,n-1):t)
                       | otherwise = converteMSet t
--39.
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet a [] = [(a,1)]
insereMSet a ((b,n):t) | (a == b) = (b,n+1) : t
                       | otherwise = (b,n) : insereMSet a t
--40.
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet a ((b,n):t) | (a == b) = if (n-1) > 0 then (b,n-1) : t else t
                       | otherwise = (b,n) : removeMSet a t
--41.
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:x:t) = aux h (x:t) 1
  where
    aux h [x] n = if (h == x) then [(h,n+1)] else (h,n) : [(x,1)]
    aux h (x:t) n | (h == x) = aux h t (n+1)
                  | otherwise = (h,n) : aux x t 1
--42.
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers (h:t) = case h of
  Left h -> (h:x,y)
  Right h -> (x,h:y)
 where
 (x,y) = partitionEithers t
--43.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of
  Just h -> h : catMaybes t
  Nothing -> catMaybes t
--44.
data Movimento = Norte
               | Sul
               | Este
               | Oeste
               deriving Show
posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao (x,y) [] = (x,y)
posicao (x,y) (h:t) = case h of
  Norte -> posicao (x,y+1) t
  Sul -> posicao (x,y-1) t
  Este -> posicao (x+1,y) t
  Oeste -> posicao (x-1,y) t
--45.
caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (x1,y1) | (x < x1) = Este : caminho (x,y) (x1-1,y1)
                      | (x > x1) = Oeste : caminho (x,y) (x1+1,y1)
                      | (y < y1) = Norte : caminho (x,y) (x1,y1-1)
                      | (y > y1) = Sul : caminho (x,y) (x1,y1+1)
                      | (x,y) == (x1,y1) = []
--46.
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (h:t) = case h of
  Norte -> vertical t
  Sul -> vertical t
  _ -> False
--47.
data Posicao = Pos Int Int
               deriving Show
maisCentral :: [Posicao] -> Posicao
maisCentral [] = error "Lista Vazia"
maisCentral [h] = h
maisCentral (h:x:t) | menor h < menor x = maisCentral (h:t)
                    | otherwise = maisCentral (x:t)
  where
    menor (Pos x y) = x^2 + y^2
--48.
vizinhos :: Posicao -> [Posicao] -> [Posicao]
vizinhos _ [] = []
vizinhos (Pos x y) ((Pos x1 y1):t) | abs(x1-x) == 1 && abs(y1-y) < 1 = (Pos x1 y1) : vizinhos (Pos x y) t
                                   | abs(x1-x) < 1 && abs(y1-y) == 1 = (Pos x1 y1) : vizinhos (Pos x y) t
                                   | otherwise = vizinhos (Pos x y) t
--49.
mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [] = error "Lista Vazia"
mesmaOrdenada [_] = True
mesmaOrdenada ((Pos _ y):(Pos x1 y1):t) | y == y1 = mesmaOrdenada ((Pos x1 y1):t)
                                        | otherwise = False
--50.
data Semaforo = Verde
              | Amarelo
              | Vermelho
              deriving Show
interseccaoOK :: [Semaforo] -> Bool
interseccaoOK (Vermelho:t) = interseccaoOK t
interseccaoOK (_:t) = aux t
  where
    aux [] = True
    aux (Vermelho:t) = aux t
    aux (_:t) = False
