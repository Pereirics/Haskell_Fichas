import Data.Char
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (h*2) : dobros t

positivos :: [Int] -> Bool
positivos [] = False
positivos [h] = if h>0 then True else False
positivos (h:t) = if h>0 then positivos t else False

soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h > 0 = h : soPos t
            | otherwise = soPos t

tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t)
   | length (h:t) <= 3 = (h:t)
   | otherwise = tresUlt t

segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t

nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros el ((x,y):t)
   | el /= x = nosPrimeiros el t
   | otherwise = True

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):[]) = (a,b,c)
sumTriplos ((a,b,c):(d,e,f):t) = sumTriplos ((a + d, b + e, c + f):t)

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if (ord(h) < 57) && (ord(h) >= 48) then h:soDigitos t else soDigitos t

nums :: String -> [Int]
nums "" = []
nums (h:t) = if (isDigit(h)) then characterToInt(h) : nums t else nums t

characterToInt :: Char -> Int
characterToInt ch = ord(ch) - 48

type Polinomio = [Monomio]
type Monomio = (Float,Int)

calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,e):t) = ((x ^ e) * c) + calcula x t

simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,e):t) = if e == 0 then simp t else (c,e) : simp t

mult :: Monomio -> Polinomio -> Polinomio
mult (_,_) [] = []
mult (c,e) ((d,f):t) = (c*d,e+f) : mult (c,e) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((c,e):t) = aux (c,e) t [] []
  where
    aux (c,e) [] [(c1,e1)] acc = ((c,e) : (c1,e1) : acc)
    aux (c,e) [] lista acc = aux (head lista) (tail lista) (tail lista) ((c,e):acc)
    aux (c,e) ((c1,e1):t) lista acc | e == e1 = aux (c+c1,e) t lista acc
                                    | otherwise = aux (c,e) t ((c1,e1):lista) acc

produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (h:t) l = mult h l ++ produto t l

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((c,e):t) = aux (c,e) t [] []
  where
    aux (c,e) [] [] acc = ((c,e):acc)
    aux (c,e) [] nusados acc = aux (head nusados) (tail nusados) [] ((c,e):acc)
    aux (c,e) ((c1,e1):t) nusados acc | e > e1 = aux (c,e) t ((c1,e1):nusados) acc
                                      | e == e1 = if c >= c1 then aux (c,e) t ((c1,e1):nusados) acc else aux (c1,e1) t ((c,e):nusados) acc
                                      | otherwise = aux (c1,e1) t ((c,e):nusados) acc

