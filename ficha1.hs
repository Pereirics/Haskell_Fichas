import Data.Char 
--1.
perimetro :: Float -> Float 
perimetro n = 2 * pi * n

dist1 :: (Double,Double) -> (Double,Double) -> Double
dist1 (x1,y1) (x2,y2) = sqrt(((x2-x1)^2) + ((y2-y1)^2))

primUlt :: [Int] -> (Int,Int)
primUlt [] = (0,0)
primUlt (h:t) = (h,last t)

multiplo :: Int -> Int -> Bool 
multiplo 0 _ = False
multiplo _ 0 = True
multiplo a b | mod b a == 0 = True 
             | otherwise = False 

truncaImpar :: [Int] -> [Int]
truncaImpar [] = []
truncaImpar (h:t) | mod ((length t) + 1) 2 == 0 = h:t
                  | otherwise = t 

max2 :: Int -> Int -> Int
max2 a b | a > b = a 
         | otherwise = b 

max3 :: Int -> Int -> Int -> Int
max3 a b c | max2 a b == a = max2 a c
           | otherwise = max2 b c

--2.
nRaizes :: Double -> Double -> Double -> Double 
nRaizes a b c | b^2 - 4*a*c > 0 = 2
              | b^2 - 4*a*c == 0 = 1
              | otherwise = 0

raizes :: Double -> Double -> Double -> [Double] 
raizes a b c | nRaizes a b c == 2 = [r1, r2]
             | nRaizes a b c == 1 = [-b / (2*a)]
             | otherwise = []
             where
                r1 = (-b + b^2 - 4*a*c)/(2*a)
                r2 = (-b - b^2 - 4*a*c)/(2*a)

--3.
type Hora = (Int, Int)

horaValida :: Hora -> Bool 
horaValida (h,m) = (0 <= h && h < 24) && (0 <= m && m < 60) 

comparaHora :: Hora -> Hora -> Bool 
comparaHora (h1,m1) (h2,m2) | h1 < h2 = True
                            | h1 == h2 && m1 < m2 = True
                            | otherwise = False

converteHoras :: Hora -> Int 
converteHoras (h,m) = h * 60 + m

converteMinutos :: Int -> Hora
converteMinutos m = (div m 60, mod m 60)

difHoras :: Hora -> Hora -> Int 
difHoras a b | comparaHora a b = converteHoras b - converteHoras a 
             | comparaHora b a = converteHoras a - converteHoras b
             | otherwise = 0

addMin :: Hora -> Int -> Hora
addMin (h,m) min | m + min < 60 = (h, m+min)
                 | m + min == 60 = (h + 1, 0)
                 | otherwise = (h + extra, mod extra1 60)
                 where 
                     extra = div (m+min) 60
                     extra1 = mod (m+min) 60 

--4.
data Hora1 = H Int Int deriving (Show,Eq)

horaValida1 :: Hora1 -> Bool 
horaValida1 (H h m) = (0 <= h && h < 24) && (0 <= m && m < 60) 

comparaHora1 :: Hora1 -> Hora1 -> Bool 
comparaHora1 (H h1 m1) (H h2 m2) | h1 < h2 = True
                            | h1 == h2 && m1 < m2 = True
                            | otherwise = False

converteHoras1 :: Hora1 -> Int 
converteHoras1 (H h m) = h * 60 + m

converteMinutos1 :: Int -> Hora1
converteMinutos1 m = H (div m 60) (mod m 60)

difHoras1 :: Hora1 -> Hora1 -> Int 
difHoras1 a b | comparaHora1 a b = converteHoras1 b - converteHoras1 a 
              | comparaHora1 b a = converteHoras1 a - converteHoras1 b
              | otherwise = 0

addMin1 :: Hora1 -> Int -> Hora1
addMin1 (H h m) min | m + min < 60 = H h (m+min)
                    | m + min == 60 = H (h + 1) 0
                    | otherwise = H (h + extra) (mod extra1 60)
                    where 
                       extra = div (m+min) 60
                       extra1 = mod (m+min) 60 

--5.
data Semaforo = Verde 
              | Amarelo
              | Vermelho
              deriving (Show,Eq)

next :: Semaforo -> Semaforo
next x | x == Verde = Amarelo
       | x == Amarelo = Vermelho
       | otherwise = Verde 

stop :: Semaforo -> Bool
stop x | x == Vermelho = True
       | otherwise = False

safe :: Semaforo -> Semaforo -> Bool 
safe x y | x == Verde && y == Vermelho = True 
         | x == Vermelho && y == Verde = True
         | otherwise = False

--6.
data Ponto = Cartesiano Double Double 
           | Polar Double Double
           deriving (Show,Eq)

posx :: Ponto -> Double 
posx ponto = case ponto of
    Cartesiano x y -> x
    Polar r a -> r * cos(r)

posy :: Ponto -> Double 
posy ponto = case ponto of 
    Cartesiano x y -> y
    Polar r a -> r * sin(r)

raio :: Ponto -> Double 
raio ponto = case ponto of 
    Cartesiano x y -> sqrt(x^2 + y^2)
    Polar r a -> r 

angulo :: Ponto -> Double 
angulo ponto = case ponto of
    Cartesiano x y -> atan(y/x)
    Polar r a -> a 

convC :: Ponto -> Ponto
convC (Polar d a) = Cartesiano (cos(a)*d) (sin(a)*d)
convC (Cartesiano x y) = Cartesiano x y

convP :: Ponto -> Ponto
convP (Cartesiano x y) = Polar (sqrt(x^2 + y^2)) (atan(y/x))
convP (Polar r a) = Polar r a 

dist :: Ponto -> Ponto -> Double 
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt((x2-x1)^2 + (y2-y1)^2)
dist _ _ = 0

--7.
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

poligono :: Figura -> Bool
poligono figura = case figura of 
    Circulo _ _ -> False
    _ -> True

vertices :: Figura ->  [Ponto]
vertices figura = case figura of 
    Circulo _ _ -> []
    Triangulo a b c-> [a,b,c]
    Rectangulo a b -> [a,Cartesiano (posx a) (posy b),Cartesiano (posx b) (posy a),b]

area :: Figura -> Double 
area (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perímetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- fórmula de Heron
area (Rectangulo p1 p2) = 
    let p3 = Cartesiano (posx p1) (posy p2)
        c = dist p1 p3 
        l = dist p2 p3
    in c * l 
area (Circulo _ p2) = pi * p2 * p2 

perimetro' :: Figura -> Double 
perimetro' (Triangulo p1 p2 p3) =
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
    in a+b+c 
perimetro' (Rectangulo p1 p2) =
    let p3 = Cartesiano (posx p1) (posy p2)
        c = dist p1 p3 
        l = dist p2 p3
    in 2*c+2*l
perimetro' (Circulo _ raio) = 2 * pi * raio

--8.
isLower' :: Char -> Bool 
isLower' c = ord(c) <= 122 && ord(c) >= 97

isDigit' :: Char -> Bool 
isDigit' c = ord(c) >= 48 && ord(c) <= 57   

isAlpha' :: Char -> Bool
isAlpha' c = (ord(c) >= 65 && ord(c) <= 90) || (ord(c) <= 122 && ord(c) >= 97)

toUpper' :: Char -> Char 
toUpper' c = chr(ord(c) - 32)

intToDigit' :: Int -> Char 
intToDigit' n = chr(n+48)

digitToInt' :: Char -> Int 
digitToInt' n = ord(n) - 48

