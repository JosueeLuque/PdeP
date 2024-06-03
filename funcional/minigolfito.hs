import Text.Show.Functions()

-- Modelo inicial
data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = Jugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = Jugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

-- maximoSegun :: ()
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b


-- Punto 1 --

-- 1.a
type Palo = Habilidad -> Tiro

putter :: Palo
putter unaHabilidad = Tiro {
    velocidad = 10,
    precision = (*2) . precisionJugador $ unaHabilidad,
    altura = 0
}

madera :: Palo
madera unaHabilidad = Tiro {
    velocidad = 100,
    precision = flip div 2 . precisionJugador $ unaHabilidad,
    altura = 5
}

hierro :: Int -> Palo
hierro n unaHabilidad = Tiro {
    velocidad = (*n) . fuerzaJugador $ unaHabilidad,
    precision = flip div 2 . precisionJugador $ unaHabilidad,
    altura = max (n-3) 0
}

-- 1.b
palos :: [Palo]
palos = [putter, madera] ++ map (hierro) [1..10]


-- Punto 2
{-
    Definir la función golpe que dados una persona y un palo, 
    obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
-}
golpe :: (Palo) -> Jugador -> Tiro
golpe unPalo =  unPalo . habilidad

-- Punto 3
{-
    Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, 
    y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. 
    En principio necesitamos representar los siguientes obstáculos:

-}

-- 3.a
data Obstaculo = Obstaculo {
    puedeSuperar :: Tiro -> Bool,
    efectoDeSuperar :: Tiro -> Tiro
}

{-
  Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, 
  independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, 
  la precisión pasa a ser 100 y la altura 0.
-}

tunelConRampita :: Obstaculo
tunelConRampita = Obstaculo superaTunelConRampita efecoTunelRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita unTiro = (precision unTiro) > 90 && vaRasDelSuelo unTiro

vaRasDelSuelo :: Tiro -> Bool
vaRasDelSuelo = (==0) . altura

efecoTunelRampita :: Tiro -> Tiro
efecoTunelRampita unTiro = cambiarAltura 0 . cambiarPrecision 100 . cambiarVelocidad (velocidad unTiro *2) $ unTiro

cambiarVelocidad :: Int -> Tiro -> Tiro
cambiarVelocidad unValor unTiro = unTiro{velocidad = unValor}

cambiarPrecision :: Int -> Tiro -> Tiro
cambiarPrecision unValor unTiro = unTiro{precision = unValor}

cambiarAltura :: Int -> Tiro -> Tiro
cambiarAltura unaAltura unTiro = unTiro{altura = unaAltura}


laguna :: Int -> Obstaculo
laguna largoDeLaguna = Obstaculo superaLaguna (efectoLaguna largoDeLaguna)

superaLaguna :: Tiro -> Bool
superaLaguna unTiro = velocidad unTiro > 80 && between 1 5 (altura unTiro)

efectoLaguna :: Int -> Tiro -> Tiro
efectoLaguna largo unTiro = cambiarAltura (div (altura unTiro) largo) $ unTiro


hoyo :: Obstaculo
hoyo = Obstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo unTiro = between 5 20 (velocidad unTiro) && vaRasDelSuelo unTiro && precision unTiro > 95

efectoHoyo :: Tiro -> Tiro
efectoHoyo = tiroDetenido

tiroDetenido :: Tiro -> Tiro
tiroDetenido = cambiarAltura 0 . cambiarPrecision 0 . cambiarVelocidad 0

esSuperable :: Obstaculo -> Tiro -> Tiro
esSuperable unObstaculo unTiro
  | (puedeSuperar unObstaculo) unTiro = (efectoDeSuperar unObstaculo) unTiro
  | otherwise = tiroDetenido unTiro

-- Punto 4 --
palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador unObstaculo = filter (leSirveParaSuperar unJugador unObstaculo) palos

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar unJugador unObstaculo unPalo = puedeSuperar unObstaculo . golpe unJugador $ unPalo


cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera unTiro [] = 0
cuantosObstaculosConsecutivosSupera unTiro (obstaculo : obstaculos)
  | (puedeSuperar obstaculo) unTiro = 1 + cuantosObstaculosConsecutivosSupera ((efectoDeSuperar obstaculo) unTiro) obstaculos
  | otherwise = 0


paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil unJugador obstaculos =  maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos . golpe unJugador) palos

{-
    Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, 
    se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. 
    Se dice que un niño ganó el torneo si tiene más puntos que los otros niños.
-}

jugadorDeTorneo = fst
puntosGanados = snd


pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = map (padre.jugadorDeTorneo) . filter (not . gano puntosDeTorneo) $ puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = all (tieneMasPuntos puntosDeUnJugador) . filter (/= puntosDeUnJugador) $ puntosDeTorneo

tieneMasPuntos :: (Jugador, Puntos) -> (Jugador, Puntos) -> Bool
tieneMasPuntos unJugador = (< puntosGanados unJugador) . puntosGanados