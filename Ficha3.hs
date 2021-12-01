import Ficha1

type Etapa = (Hora1,Hora1)
type Viagem = [Etapa]

etapaVal :: Etapa -> Bool
etapaVal (h1,h2) = if horaValida1(h1) && horaValida1(h2) then comparaHora1 (h1) (h2) else False

viagemVal :: Viagem -> Bool
viagem [h] = etapaVal h
viagemVal ((h1,h2):(h3,h4):t) = etapaVal (h1,h2) && etapaVal (h3,h4) && comparaHora1 h2 h3

partCheg :: Viagem -> Etapa
partCheg [h] = h
partCheg ((h1,_):t:[(_,h2)]) = (h1,h2)

tempoVia :: Viagem -> Int
tempoVia [] = 0
tempoVia [(h1,h2)] = if etapaVal (h1,h2) then converteHoras1 (h2) - converteHoras1 (h1) else 0
tempoVia ((h1,h2):t) = if viagemVal ((h1,h2):t) then converteHoras1 (h2) - converteHoras1 (h1) + tempoVia t else 0

tempoEsp :: Viagem -> Int
tempoEsp [] = 0
tempoEsp [(h1,h2)] = 0
tempoEsp ((h0,h1):(h2,h01):t) = if viagemVal ((h0,h1):(h2,h01):t) then converteHoras1 (h2) - converteHoras1 (h1) + tempoEsp ((h2,h01):t) else 0

tempoTot :: Viagem -> Int
tempoTot v = tempoVia v + tempoEsp v

type Poligonal = [Ponto]

compLi :: Poligonal -> Double
compLi [] = 0
compLi [h] = 0
compLi (h:x:t) = (dist h x) + compLi (x:t)

fechada :: Poligonal -> Bool
fechada [h] = False
fechada (h:t) = h == (last t)

triangula :: Poligonal -> [Figura]
triangula [p1,p2,p3] = [Triangulo p1 p2 p3]
triangula (p1:p2:p3:t) = Triangulo p1 p2 p3 : triangula (p1:p3:t)

areaLi :: Poligonal -> Double
areaLi [p1,p2,p3] = area (Triangulo p1 p2 p3)
areaLi (p1:p2:p3:t) = area (Triangulo p1 p2 p3) + areaLi (p1:p3:t)

mover :: Poligonal -> Ponto -> Poligonal
mover ((Cartesiano p1 p2):t) (Cartesiano p3 p4) = aux ((Cartesiano p1 p2):t) (p1-p3,p2-p4)
  where
    aux [Cartesiano p1 p2] (dx,dy) = [Cartesiano (p1-dx) (p2-dy)]
    aux ((Cartesiano p1 p2):t) (dx,dy) = (Cartesiano (p1-dx) (p2-dy)) : aux t (dx,dy)

zoom :: Double -> Poligonal -> Poligonal
zoom n [] = []
zoom n (h:t) = case h of
  Cartesiano x y -> Cartesiano (n*x) (n*y) : zoom n t
  Polar d a -> Polar (d*n) a : zoom n t

data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail n email ((n1,contacto):t) | n == n1 = (n1, aux email contacto):t
                                      | otherwise = (n1,contacto) : acrescEmail n email t
  where
    aux email [] = [Email email]
    aux email (c:t) = case c of
      Email c -> if c == email then (Email c:t) else (Email c) : aux email t
      _ -> c : aux email t

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails n ((n1,c):t) | n == n1 = Just (aux c)
                       | otherwise = verEmails n t
  where
    aux [] = []
    aux (c:t) = case c of
      Email c -> c : aux t
      _ -> aux t

consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (c:t) = case c of
  Casa c -> c : consTelefs t
  Trab c -> c : consTelefs t
  Tlm c -> c : consTelefs t
  _ -> consTelefs t

casa :: Nome -> Agenda -> Maybe Integer
casa _ [] = Nothing
casa n ((n1,c):t) | n == n1 = aux c
                  | otherwise = casa n t
  where
    aux [] = Nothing
    aux (h:t) = case h of
      Casa h -> Just h
      _ -> aux t

type Dia = Int
type Mes = Int
type Ano = Int
data Data = D Dia Mes Ano
          deriving Show
type TabDN = [(Nome,Data)]

procura :: Nome -> TabDN -> Maybe Data
procura _ [] = Nothing
procura n ((n1,d):t) | n == n1 = Just d
                     | otherwise = procura n t

idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d m a) n ((n1,nasc):t) | n == n1 = aux (D d m a) nasc
                                | otherwise = idade (D d m a) n t
  where
    aux (D d m a) (D d1 m1 a1) | a >= a1 && m > m1 || a >= a1 && m >= m1 && d >= d1 = Just (a-a1)
                               | otherwise = Just (a-a1-1)

anterior :: Data -> Data -> Bool
anterior (D d m a) (D d1 m1 a1) = a1 > a || a1 >= a && m1 > m || a1 >= a && m1 > m && d1 > d

ordena :: TabDN -> TabDN
ordena tabela = aux tabela
  where
    aux [] = []
    aux tabela = velho tabela : aux (removeD (velho tabela) tabela)

velho :: TabDN -> (Nome,Data) -- auxiliar para calcular qual é o mais velho em TabDN
velho ((n,nas):[]) = (n,nas)
velho ((n,nas):(n1,nas1):t) | anterior nas nas1 = velho ((n,nas):t)
                            | otherwise = velho ((n1,nas1):t)

removeD :: (Nome,Data) -> TabDN -> TabDN -- auxiliar para remover uma pessoa de uma TabDN
removeD (n,nas) [] = []
removeD (n,nas) ((n1,nas1):t) = if n == n1 then t else (n1,nas1) : removeD (n,nas) t

porIdade :: Data -> TabDN -> [(Nome,Int)]
porIdade nas tab = aux nas (ordena tab)
  where
    aux nas [] = []
    aux nas ((n1,nas1):t) = (idade1 nas (n1,nas1)) : aux nas t

idade1 :: Data -> (Nome,Data) -> (Nome,Int) -- auxiliar para calcula a idade de uma pessoa numa dada data
idade1 (D d m a) (n,D d1 m1 a1) | a >= a1 && m > m1 || a >= a1 && m >= m1 && d >= d1 = (n,(a-a1))
                                | otherwise = (n,(a-a1-1))

data Movimento = Credito Float | Debito Float
               deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ []) _ = []
extValor (Ext i (((D d m a),desc,mov):t)) valor = case mov of
  Credito c -> if c > valor then (Credito c) : extValor (Ext i t) valor else extValor (Ext i t) valor
  Debito c ->  if c > valor then (Debito c) : extValor (Ext i t) valor else extValor (Ext i t) valor

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ []) _ = []
filtro (Ext i ((dataa,desc,mov):t)) lista | elemDesc desc lista = (dataa,mov) : filtro (Ext i t) lista
                                          | otherwise = filtro (Ext i t) lista

elemDesc :: String -> [String] -> Bool -- auxiliar para determinar de a descrição de um movimento pertence à lista de strings dada (PODIA TER USADO O ELEM NORMAL)
elemDesc _ [] = False
elemDesc (h:t) ((x:xs):y) | h == x = aux t xs
                          | otherwise = elemDesc (h:t) y
  where
    aux [] [] = True
    aux _ [] = False
    aux (h:t) (x:xs) | h == x = aux t xs
                     | otherwise = False

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ []) = (0,0)
creDeb (Ext i ((_,_,mov):t)) = case mov of
  Credito c -> (c+x,y)
  Debito c -> (x,c+y)
 where
   (x,y) = creDeb (Ext i t)

saldo :: Extracto -> Float
saldo (Ext i lista) = i-(x+y)
  where
    (x,y) = creDeb (Ext i lista)










