import Data.Char
--Ficha 1
-- perimetro– que calcula o perımetro de uma circunferˆencia, dado o comprimentodo seu raio.
perimetro :: Float -> Float
perimetro raio = 2 * pi * raio

--(b)dist– que calcula a distˆancia entre dois pontos no plano Cartesiano.  Cada ponto ́e um par de valores do tipoDouble.
dist :: (Double, Double) -> (Double, Double) -> Double
dist ponto1 ponto2 = sqrt ((fst ponto1 - fst ponto2)^2 + (snd ponto1 - snd ponto2)^2)

--(c)primUlt–  que  recebe  uma  lista  e  devolve  um  par  com  o  primeiro e o ́ultimo elemento dessa lista.
primUlt :: (Eq a) => [a] -> [a]
primUlt lista = case lista of
    []  -> error "A lista não pode ser vazia, inclua pelo menos dois elementos."
    [x] -> error "A lista tem de ter dois elementos."
    otherwise -> [head lista, last lista]

--(d)multiplo – tal que multiplo m n testa se o numero inteiro m ́e multiplo de n.
multiplo :: Integer -> Integer -> Bool
multiplo mult numero = if (mult `mod` numero) == 0
    then True
    else False

--(e)truncaImpar– que recebe uma lista e, se o comprimento da lista for ́ımpar retira-lhe o primeiro elemento, caso contrario devolve a propria lista.
truncaImpar :: [a] -> [a]
truncaImpar lista = if (length (lista) `mod` 2) == 0
    then lista
    else tail lista

--(f)max2– que calcula o maior de dois numeros inteiros.
max2 :: Integer -> Integer -> Integer
max2 numero1 numero2
    | numero1 == numero2 = numero1
    | otherwise = if numero1 > numero2 then numero1 else numero2

--(g)max3– que calcula o maior de três numeros inteiros, usando a função max2
max3 :: Integer -> Integer -> Integer -> Integer
max3 numero1 numero2 numero3 = max2 (max2 numero1 numero2) numero3

-- A funcao nRaizes que recebe os (3) coeficientes de um polinomio de 2ograu e que calcula o numero de raızes (reais) desse polinomio
binomio :: (Float, Float, Float) -> Float
binomio (a, b, c) = b^2 - 4*a*c

nRaizes :: (Float, Float, Float) -> Integer
nRaizes (a, b, c) | b == 0 = 1
                  | b > 0 = 2
                  | b < 0 = 0
                  where b = binomio (a, b, c)

-- funcao raizes que, usando a funcao anterior, recebe os coeficientes do polinomio e calcula a lista das suas raızes reais.
raizes :: (Float, Float, Float) -> (Float,Float)
raizes (a, b, c) = if d == 0 then error "Só tem raízes imaginárias" else (x, y)
               where
                    x = ((-b) + sqrt d) / (2*a)
                    y = ((-b) - sqrt d) / (2*a)
                    d = binomio (a, b, c)
-- Horas
type Hora = (Int, Int)

validarHora :: Hora -> Bool
validarHora (horas, minutos) = if (horas >= 0 && horas < 24) && (minutos >= 0 && minutos < 60)
    then True
    else False

verificarHoraPosterior :: (Hora, Hora) -> Bool
verificarHoraPosterior (horaPosterior, horaAnterior) = if validarHora horaPosterior && validarHora horaAnterior then resultado else error "Hora inválida fornecida"
    where
        resultado = if posterior > anterior then True else False
        posterior = horaParaMinutos horaPosterior
        anterior  = horaParaMinutos horaAnterior

horaParaMinutos :: Hora -> Int
horaParaMinutos (horas, minutos) = horas*60 + minutos

minutosParaHoras :: Int -> Hora
minutosParaHoras minutos = (horas, minutosRestantes)
    where
        horas = quot minutos 60
        minutosRestantes = mod minutos 60

diferencaHoras :: (Hora, Hora) -> Int
diferencaHoras (horaA, horaB) = if validarHora horaA && validarHora horaB then resultado else error "Hora inválida fornecida"
    where
        resultado = (max a b) - (min a b)
        a = horaParaMinutos horaA
        b = horaParaMinutos horaB

-- Semaforo
data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

next :: Semaforo -> Semaforo
next estado | estado == Verde    = Amarelo
            | estado == Amarelo  = Vermelho
            | estado == Vermelho = Verde

stop :: Semaforo -> Bool
stop estado = if estado == Vermelho then True else False

safe :: Semaforo -> Semaforo -> Bool
safe semaforo1 semaforo2 = if semaforo1 == Vermelho || semaforo2 == Vermelho then True else False


-- Pontos
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show, Eq)

polarParaCartesiano :: Ponto -> Ponto
polarParaCartesiano (Polar comprimento angulo) = Cartesiano x y
    where x = comprimento * (cos angulo)
          y = comprimento * (sin angulo)

posx :: Ponto -> Double
posx (Polar comprimento angulo) = let
    (Cartesiano x y) = polarParaCartesiano (Polar comprimento angulo)
    in if x < 0 then (-x) else x
posx (Cartesiano x y) = if x < 0 then (-x) else x

posy :: Ponto -> Double
posy (Polar comprimento angulo) = let
    (Cartesiano x y) = polarParaCartesiano (Polar comprimento angulo)
    in if y < 0 then (-y) else y
posy (Cartesiano x y) = if y < 0 then (-y) else y

raio :: Ponto -> Double
raio (Polar comprimento angulo) = comprimento
raio (Cartesiano x y) = sqrt (x^2 + y^2)

angulo :: Ponto -> Double
angulo (Polar comprimento angulo) = angulo
angulo (Cartesiano x y) = atan(y/x)

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

-- Figuras
data Figura = Circulo Ponto Double| Rectangulo Ponto Ponto| Triangulo Ponto Ponto Ponto deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono otherwise     = True

vertices :: Figura -> [Ponto]
vertices (Circulo _ _) = error "O circulo não tem vértices"
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]
vertices (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) = [p1, p2, p3, p4]
    where
        p1 = (Cartesiano x1 y1)
        p2 = (Cartesiano x1 y2)
        p3 = (Cartesiano x2 y1)
        p4 = (Cartesiano x2 y2)

area :: Figura -> Double
area (Circulo _ raio) = pi * raio^2
area (Triangulo p1 p2 p3) =
    let a = distPonto p1 p2
        b = distPonto p2 p3
        c = distPonto p3 p1
        s = (a + b + c) / 2 -- semi-perimetro
        in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) =
    let
        base = x2 - x1
        altura = y2 - y1
        area = base * altura
        in if area < 0 then (-area) else area

perimetroFigura :: Figura -> Double
perimetroFigura (Circulo _ raio) = 2 * pi * raio
perimetroFigura (Triangulo p1 p2 p3) =
    let a = distPonto p1 p2
        b = distPonto p2 p3
        c = distPonto p3 p1
        in a + b + c
perimetroFigura (Rectangulo (Cartesiano x1 y1) (Cartesiano x2 y2)) =
    let
        base = x2 - x1
        altura = y2 - y1
        perimetro = (base + altura) * 2
        in if perimetro < 0 then (-perimetro) else perimetro

isMinuscula :: Char -> Bool
isMinuscula carater = elem (ord carater) [97..122]

isDigito :: Char -> Bool
isDigito carater = elem (ord carater) [48..57]

isLetra :: Char -> Bool
isLetra carater = elem (ord carater) [65..90] || isMinuscula carater

paraMaiuscula :: Char -> Char
paraMaiuscula letra = if isMinuscula letra
    then chr (ord letra - 32)
    else error "Insira uma letra minuscula"

inteiroParaDigito :: Int -> Char
inteiroParaDigito numero = if elem numero [0..9]
    then chr (numero + 48)
    else error "Insira um numero"

digitoParaInteiro :: Char -> Int
digitoParaInteiro digito = if isDigito digito
    then ord digito - 48
    else error "Insira um digito"
