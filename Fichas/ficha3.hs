-- Exercicio 1
--array de testes [(H 9 30, H 10 25), (H 11 20, H 12 45), (H 13 30, H 14 45)]
data Hora = H Int Int deriving Show

type Etapa = (Hora, Hora)
type Viagem = [Etapa]

validaHora (H horas minutos) = (horas >= 0 && horas < 24) && (minutos >= 0 && minutos < 60) 
horaEmMinutos hora@(H a b) = a*60 + b
minutosEmHoras hora = (quot hora 60, mod hora 60)

validaEtapa :: Etapa -> Bool
validaEtapa (hora1, hora2) = validaHora hora1 && validaHora hora2 && (horaEmMinutos hora2 > horaEmMinutos hora1)

validaViagem :: Viagem -> Bool
validaViagem [x] = validaEtapa x
validaViagem (x:y:xs) = if validaEtapa x && (horaEmMinutos $ fst y) > (horaEmMinutos $ snd x)
  then validaViagem (y:xs)
  else False

partidaChegada :: Viagem -> (Hora, Hora)
partidaChegada viagem = (fst $ head viagem, snd $ last viagem)

tempoEtapa (a, b) = let
  minutosIniciais = horaEmMinutos a
  minutosFinais = horaEmMinutos b
  total = minutosFinais - minutosIniciais
  in minutosEmHoras total

tempoViagem :: Viagem -> (Int, Int)
tempoViagem viagem = minutosEmHoras $ sum [ horaEmMinutos b - horaEmMinutos a | (a, b) <- viagem ]

tempoEspera :: Viagem -> (Int, Int)
tempoEspera viagem = minutosEmHoras $ aux viagem
  where aux [x] = 0
        aux (x:y:xs) = ((horaEmMinutos $ snd y) - (horaEmMinutos $ fst x)) + aux (y:xs)

-- Exercicio 2
-- teste [(Cartesiano 2 (-3)), (Cartesiano 4 5), (Cartesiano 3 (-4))]
data Ponto = Cartesiano Double Double
           | Polar Double Double 
             deriving (Show, Eq)

type Poligonal = [Ponto]

data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

dist :: (Double, Double) -> (Double, Double) -> Double
dist ponto1 ponto2 = sqrt ((fst ponto1 - fst ponto2)^2 + (snd ponto1 - snd ponto2)^2)

distPonto :: Ponto -> Ponto -> Double
distPonto (Cartesiano x1 y1) (Cartesiano x2 y2) = dist ponto1 ponto2
    where
        ponto1 = (x1, y1)
        ponto2 = (x2, y2)
distPonto (Cartesiano x1 y1) (Polar x2 y2) = dist ponto1 ponto2
    where
        ponto1 = (x1, y1)
        ponto2 = (x2 * (cos y2), x2 * (sin y2))
distPonto (Polar x1 y1) (Cartesiano x2 y2) = dist ponto1 ponto2
    where
        ponto1 = (x1 * (cos y1), x1 * (sin y1))
        ponto2 = (x2, y2)
distPonto (Polar x1 y1) (Polar x2 y2) = dist ponto1 ponto2
    where
        ponto1 = (x1 * (cos y1), x1 * (sin y1))
        ponto2 = (x2 * (cos y2), x2 * (sin y2))

area (Triangulo p1 p2 p3) =
    let a = distPonto p1 p2
        b = distPonto p2 p3
        c = distPonto p3 p1
        s = (a + b + c) / 2 -- semi-perimetro
        in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron

compLinha :: Poligonal -> Double
compLinha [x] = 0
compLinha (x:y:xs) = distPonto x y + compLinha (y:xs)

isPoligonalFechado :: Poligonal -> Bool
isPoligonalFechado poligonal = head poligonal == last poligonal

triangula :: Poligonal -> [Figura]
triangula [x,y,z] = [(Triangulo x y z)]
triangula (x:y:z:xs) = (Triangulo x y z):triangula (x:z:xs)

areaPoligonal :: Poligonal -> Double
areaPoligonal poligono = sum $ map area $ triangula poligono

mover :: Poligonal -> Ponto -> Poligonal
mover linha@((Cartesiano x y):xs) (Cartesiano x1 y1) = let
  difX = x-x1
  difY = y-y1
  in [(Cartesiano (a-difX) (b-difY)) | (Cartesiano a b) <- linha ]       

zoom :: Double -> Poligonal -> Poligonal
zoom _ [] = []
zoom factor (x:(Cartesiano a b):xs) = let 
  a1 = a*factor
  b1 = b*factor
  in x : zoom factor ((Cartesiano a1 b1):xs)

-- Exercicio 3
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show

type Nome = String
type Agenda = [(Nome, [Contacto])]
-- Testes [("ze", [Tlm 987654321, Casa 987654321, Email "teste@teste.com"]), ("David", [Tlm 987654321, Casa 987654321])]

encontrarContato _ [] = ("",[])
encontrarContato alvo ((nome, contacto):xs) = if alvo == nome 
  then (nome, contacto)
  else encontrarContato alvo xs

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email [] = [(nome, [(Email email)])]
acrescEmail nome email (x:xs) = if (fst x) == nome 
  then (a, (Email email):b):xs
  else x : acrescEmail nome email xs
    where (a, b) = x

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails alvo agenda = emails $ encontrarContato alvo agenda
  where emails (_, contacto) = if (length [ x | (Email x) <- contacto]) == 0
          then Nothing
          else Just [ x | (Email x) <- contacto]

consTelefs :: [Contacto] -> [Integer]
consTelefs contactos = [ x | (Tlm x) <- contactos ] ++ [ x | (Casa x) <- contactos ] ++ [ x | (Trab x) <- contactos ]

casa :: Nome -> Agenda -> Maybe Integer
casa nome agenda = telCasa $ encontrarContato nome agenda
  where telCasa (_, contacto) = if (length [ x | (Casa x) <- contacto]) == 0
          then Nothing
          else Just $ head [ x | (Casa x) <- contacto]


--Exercicio 4
-- [("Nome", D 31 1 1000), ("Nomi", D 31 1 1001), ("Nomo", D 31 1 1020), ("Nomu", D 31 1 1100)]
type Dia = Int
type Mes = Int
type Ano = Int
--type Nome = String

--data Data = D Dia Mes Ano deriving Show

type TabDN = [(Nome, Data)]

procura :: Nome -> TabDN -> Maybe Data
procura nome [] = Nothing
procura nome ((a,b):xs) = if a == nome then Just b else procura nome xs

idade :: Data -> Nome -> TabDN -> Maybe Int
idade (D d m a) nome tabela = let
  Just (D x y z) = procura nome tabela
  anos | m > y            = a - z
       | m == y && d >= x = a - z
       | otherwise        = a - z - 1
  in if anos >= 0 then Just anos else Nothing

anterior :: Data -> Data -> Bool
anterior (D x y z) (D d m a) = let
  anos | m > y            = a - z
       | m == y && d >= x = a - z
       | otherwise        = a - z - 1
  in if anos >= 0 then True else False

quicksort :: (Ord a, Ord b) => [(a, b)] -> [(a, b)]    
quicksort [] = []    
quicksort (x@(a, b):xs) =
  let smallerSorted = quicksort [(c, d) | (c, d) <- xs, d <= b]
      biggerSorted  = quicksort [(c, d) | (c, d) <- xs, d >  b]
  in  smallerSorted ++ [x] ++ biggerSorted

--ordena :: TabDN -> TabDN
--ordena tabela = 

porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade date tabela = quicksort filtro
  where aux = [(nome, x) | (nome, _) <- tabela, let x = (idade date nome tabela), x /= Nothing ]
        filtro = [ (nome, idade) | (nome, Just idade) <- aux]

-- Exercicio 5
data Movimento = Credito Float | Debito Float deriving Show
data Data = D Int Int Int deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]deriving Show

extValor :: Extracto -> Float -> [Movimento]
extValor (Ext _ lista) valor  = aux ++ aux2
    where
        aux  = [ mov | (_,_,mov@(Credito x)) <- lista, x > valor ]
        aux2 = [ mov | (_,_,mov@(Debito x)) <- lista, x > valor ]

filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext _ lista) descricao = [ (a, b) | (a, x, b) <- lista, elem x descricao]

creDeb :: Extracto -> (Float,Float)
creDeb (Ext _ lista) = (sum aux, sum aux2)
    where
        aux  = [ cre | (_,_,(Credito cre)) <- lista]
        aux2 = [ deb | (_,_,(Debito deb)) <- lista]

saldo :: Extracto -> Float
saldo extracto@(Ext x lista) = let (a,b) = creDeb extracto in x + b - a