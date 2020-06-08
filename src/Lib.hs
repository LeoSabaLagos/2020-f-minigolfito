module Lib where
import Text.Show.Functions

laVerdad = True

--------------------------------------------------------------------------------------------------------------------
-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
--bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Punto 1
type Palo = Habilidad -> Tiro 

-- Habilidades de ejemplo
habilidad1 = Habilidad 100 2
habilidad2 = Habilidad 500 6
habilidad3 = Habilidad 150 30

-- a)

-- El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
putter :: Palo 
putter habilidad = UnTiro {
    velocidad = 10, 
    precision = (precisionJugador habilidad) * 2, 
    altura = 0
    }

-- La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión
madera :: Palo
madera habilidad = UnTiro{
    velocidad = 100,
    precision = div (precisionJugador habilidad) 2,
    altura = 5
}

-- Los hierros, que varían del 1 al 10 (número al que denominaremos n), 
--  generan un tiro de velocidad igual a la fuerza multiplicada por n, 
--  la precisión dividida por n y 
--  una altura de n-3 (con mínimo 0)

hierro :: Int -> Palo
hierro n habilidad = UnTiro{
    velocidad = (fuerzaJugador habilidad) * n,
    precision =  div (precisionJugador habilidad) n,
    altura = max (n-3) 0
}

-- b)
listaPalosHierros :: [Int] -> [Palo]
listaPalosHierros [] = []
listaPalosHierros (cabeza : cola) = [hierro cabeza] ++ listaPalosHierros cola

unoAlDiez :: [Int]
unoAlDiez = [1..10]

palos :: [Palo]
palos = [putter,madera] ++ listaPalosHierros unoAlDiez
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Punto 2
-- Definir la función golpe que dados una persona y un palo, 
-- obtiene el tiro resultante de usar ese palo con las habilidades de la persona

golpe :: Palo -> Jugador -> Tiro
golpe = crearTiro 
-- golpe palo jugador = crearTiro palo jugador

crearTiro :: Palo -> Jugador -> Tiro

crearTiro putter = putter.habilidad
crearTiro madera = madera.habilidad
crearTiro palo = palo.habilidad 

-- Punto 3

data Obstaculo = UnTunelRampita | UnaLaguna{largo::Int} | UnHoyo

-- Obstaculos de ejemplo
tunelDespiadado = UnTunelRampita
lagunaNess = UnaLaguna 500
hoyoVeinte = UnHoyo

-- Criterio
type Criterio = Tiro -> Bool

tiroNoSuperador :: Tiro
tiroNoSuperador = UnTiro 0 0 0

superaObstaculo :: Obstaculo -> Tiro -> Tiro 
superaObstaculo UnTunelRampita = superaTunelRampita 
superaObstaculo UnHoyo = superaHoyo 
superaObstaculo (UnaLaguna largo) = superaLaguna (UnaLaguna largo)

superaTunelRampita :: Tiro -> Tiro
superaTunelRampita tiro
    | criterioTunelRampita tiro = UnTiro{velocidad = (velocidad tiro) *2, precision = 100, altura=0}
    | otherwise = tiroNoSuperador

criterioTunelRampita :: Criterio -- Tiro -> Bool
criterioTunelRampita tiro = precision tiro > 90 && altura tiro == 0    

superaLaguna :: Obstaculo -> Tiro -> Tiro
superaLaguna laguna tiro 
    | criterioLaguna tiro = UnTiro{velocidad = velocidad tiro, precision = precision tiro, altura = div (altura tiro) (largo laguna)}
    | otherwise = tiroNoSuperador

criterioLaguna :: Criterio -- Tiro -> Bool
criterioLaguna tiro = velocidad tiro > 80 && altura tiro >=1 && altura tiro <=5

superaHoyo :: Tiro -> Tiro
superaHoyo _ = tiroNoSuperador

criterioHoyo :: Criterio -- Tiro -> Bool
criterioHoyo tiro = velocidad tiro >= 5 && velocidad tiro <= 20 && altura tiro == 0 && precision tiro > 95 
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Punto 4
-- a)

{- palos :: [Palo]
palos = [putter,madera] ++ listaPalosHierros unoAlDiez -}

--superaObstaculo :: Obstaculo -> Tiro -> Tiro 

{- palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (superaObstaculo obstaculo (golpe jugador)) palos -}

-- b)

obstaculosConsecutivos ::  Tiro -> [Obstaculo] -> Int
obstaculosConsecutivos tiro obstaculos = length (takeWhile(criterioObstaculo tiro) obstaculos)

-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile (<3) [1,2,3,4,5]

criterioObstaculo :: Tiro -> Obstaculo -> Bool
criterioObstaculo tiro UnTunelRampita = criterioTunelRampita tiro
criterioObstaculo tiro (UnaLaguna largo) = criterioLaguna tiro
criterioObstaculo tiro UnHoyo = criterioHoyo tiro

--Lista de obstaculos de ejemplo
tunel1 :: Obstaculo
tunel1 = UnTunelRampita

tunel2 :: Obstaculo
tunel2 = UnTunelRampita

hole :: Obstaculo
hole = UnHoyo

obstabootys :: [Obstaculo]
obstabootys = [tunel1,tunel2,hole]

-- Tiro de ejemplo
tiro4b = UnTiro 10 95 0

ejercicio4B = obstaculosConsecutivos tiro4b obstabootys
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Punto 5
type Puntos = Int

padresPerdedores :: [(Jugador,Puntos)] -> [String] 
padresPerdedores [] = []
padresPerdedores (cabeza : cola) 
  | any (>snd cabeza) (map snd cola) = [obtenerPadre (fst cabeza)] ++ padresPerdedores cola
  | otherwise = padresPerdedores cola

obtenerPadre :: Jugador -> String
obtenerPadre = padre

--listaPadres = [(bart,999),(todd,200),(rafa,100)]
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------