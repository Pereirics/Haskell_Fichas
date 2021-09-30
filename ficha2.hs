import Data.Char
{-
1.
a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

funA [2,3,5,1] -> 2^2 + funA [3,5,1] -> 4 + 3^2 + funA[5,1] -> 4 + 9 + 5^2 + funA [1] -> 13 + 25 + 1 + funA[] -> 39.0

b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)==0 then h : (funB t)
                             else (funB t)

funB [8,5,12] -> [8,12]

c)
funC (x:y:t) = funC t
funC [x] = []
funC [] = []

func [1,2,3,4,5] -> []

d)
funD l = g [] l
g l [] = l
g l (h:t) = g (h:l) t

funD "otrec" = g [] "otrec"
g [] "otrec" -> g "o" "trec" -> g "to" "rec" -> ... -> g "certo" -> "certo"
-}

--2.
--a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h*2 : dobros t

--b)
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre n (h:t) | (n == h) = 1 + numOcorre n t
                  | otherwise = numOcorre n t

--c)
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) | (h > 0) = positivos t
                | otherwise = False

--d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) | h > 0 = h : soPos t
            | otherwise = soPos t

--e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) | (h < 0) = h + somaNeg t
              | otherwise = somaNeg t

--f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) | length (h:t) <= 3 = (h:t)
              | otherwise = tresUlt t

--g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((a,b):t) = b : segundos t

--h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros x ((a,b):t) | (x == a) = True
                         | otherwise = nosPrimeiros x t

--i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):[]) = (a,b,c)
sumTriplos ((a,b,c):(d,e,f):t) = sumTriplos ((a+d,b+e,c+f):t)

--3.
type Polinomio = [Monomio]
type Monomio = (Float,Int)
--a)
conta :: Int -> Polinomio -> Int
conta n [] = 0
conta n ((m,e):t) | (n == e) = 1 + conta n t
                  | otherwise = conta n t

--b)
grau :: Polinomio -> Int
grau [] = 0
grau ((n,e):[]) = e
grau ((n,e):(n1,e1):t) | e > e1 = grau ((n,e):t)
                       | otherwise = grau ((n1,e1):t)

--c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau x ((n,e):t) | (e == x) =  (n,e) : selgrau x t
                    | otherwise = selgrau x t

--d)
{- NAO COMPILA A PARTE DO FROMINTEGRAL
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((n,e):t) | n == 0 = deriv t
                | otherwise = (n*(fromIntegral e,e-1)) : deriv t
-}

--e
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((n,e):t) = n*x^e + calcula x t

--f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((n,e):t) | (n == 0) = simp t
               | otherwise = (n,e) : simp t

--g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (n,e) ((n1,e1):t) = (n*n1,e+e1) : mult (n,e) t

--h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((n,e):[]) = [(n,e)]
normaliza ((n,e):(n1,e1):t) | e == e1 = normaliza((n+n1,e):t)
                            | conta e t == 0 = (n,e) : normaliza((n1,e1):t)
                            | otherwise = normaliza ((n,e):t ++ [(n1,e1)])

--i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1++p2)

--j)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (h:t) l = mult h l ++ produto t l

--k)NÃO CONSEGUI
{-
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((n,e):(n1,e1):t) | e1 < e = produto ((n1,e1):(n,e):t)

--l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena(normaliza(p1)) == ordena(normaliza(p2))
-}





