--1.
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b | a < b = a : enumFromTo' (a+1) b
                | a == b = [a]
                | otherwise = []
--2.
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' m s n | (m <= s) && (s <= n) = m : enumFromThenTo' s (2*s - m) n
                      | (m >= s) && (s >= n) = m : enumFromThenTo' s (2*s - m) n
                      | otherwise = [m]
--3.
concatena :: [a] -> [a] -> [a]
concatena l [] = l
concatena [] l = l
concatena (h:t) (x:xs) = h : concatena t (x:xs)
--4.
pos :: [a] -> Int -> a
pos [] _ = error"Posição não encontrada"
pos (h:t) x | (x == 0) = h
            | otherwise = pos t (x-1)
--5.
reverte :: [a] -> [a]
reverte [] = []
reverte (h:[]) = [h]
reverte (h:t) = (last t) : reverte (h:init(t))
--6.
manter :: Int -> [a] -> [a]
manter a [] = []
manter 0 _ = []
manter a (h:t) =  h : manter (a-1) t
--7.
tira :: Int -> [a] -> [a]
tira _ [] = []
tira 0 l = l
tira a (h:t) = tira (a-1) t
--8.
zipado :: [a] -> [b] -> [(a,b)]
zipado _ [] = []
zipado [] _ = []
zipado (h:t) (x:xs) = (h,x) : zipado t xs
--9.
replica :: Int -> a -> [a]
replica 0 _ = []
replica n a = a : replica (n-1) a
--10.
saltinhos :: a -> [a] -> [a]
saltinhos a [] = []
saltinhos _ (h:[]) = [h]
saltinhos a (h:t) = h : a : saltinhos a t
--11.
grupos :: Eq a => [a] -> [[a]]
grupos [] = [[]]
grupos (h:t) = grupos_aux [h] h t
  where
    grupos_aux acc _ [] = [acc]
    grupos_aux acc h (x:xs) | (h==x) = grupos_aux (acc ++ [x]) x (xs)
                            | otherwise = acc : grupos_aux [x] x xs
--12.
concatenar :: [[a]] -> [a]
concatenar [] = []
concatenar (h:t) = h ++ concatenar t
--13.
iniciais :: [a] -> [[a]]
iniciais [] = [[]]
iniciais (h:t) = iniciais_aux [] h t
  where
    iniciais_aux acc h [] = acc : [acc ++ [h]]
    iniciais_aux acc h (x:xs) = acc : iniciais_aux (acc ++ [h]) x xs
--14.
caudas :: [a] -> [[a]]
caudas [] = [[]]
caudas (h:t) = (h:t) : caudas (t)
--15.
cabecas :: [[a]] -> [a]
cabecas [] = []
cabecas ([]:t) = cabecas t
cabecas ((h:t):xs) = h : cabecas xs
--16.
--total :: [[a]] -> Int

--22.
prefixo :: Eq a => [a] -> [a] -> Bool
prefixo [] _ = True
prefixo _ [] = False
prefixo (h:t) (x:xs) = if (h==x) then prefixo t xs else False
--23.
sufixo :: Eq a => [a] -> [a] -> Bool
sufixo [] _ = True
sufixo _ [] = False
sufixo l1 l2 = if (last l1 == last l2) then sufixo (init l1) (init l2) else False
--24.
subseq :: Eq a => [a] -> [a] -> Bool
subseq [] []  = True
subseq _ [] = False
subseq [] _ = True
subseq (h:t) l2 = if (elem h l2) then subseq t l2 else False
  where
    elem h [] = False
    elem h (x:xs) | h == x = True
                  | otherwise = elem h xs
--25.
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices x (h:t) = aux 0 [] x (h:t)
  where
    aux pos acc x [] = acc
    aux pos acc x (h:t) = if x == h then aux (pos+1) (acc ++ [pos]) x t else aux (pos+1) acc x t
--26.
nub' :: Eq a => [a] -> [a]
nub' (h:t) = aux_nub [] h t
  where
    aux_nub acc h [] = if ele h acc then acc else (acc ++ [h])
    aux_nub acc h t  = if ele h acc then aux_nub acc (head t) (tail t) else aux_nub (acc ++ [h]) (head t) (tail t)
    ele h [] = False
    ele h (x:xs) = if h == x then True else ele h xs
--27.
apaga :: Eq a => a -> [a] -> [a]
apaga _ [] = []
apaga x (h:t) | x == h = t
              | otherwise = h : apaga x t
--28.
apagaV :: Eq a => [a] -> [a] -> [a]
apagaV l [] = l
apagaV (h:t) (x:xs) | elem h (x:xs) = apagaV t (apaga1 h (x:xs))
                    | otherwise = h : apagaV t (x:xs)
  where
    apaga1 _ [] = []
    apaga1 h (x:xs) | h == x = xs
                    | otherwise = x : apaga1 h xs
--29.
uniao :: Eq a => [a] -> [a] -> [a]
uniao l [] = l
uniao [] _ = []
uniao (h:t) (x:xs) | elem x (h:t) = uniao (h:t) xs
                   | otherwise = uniao (h:t++[x]) xs















