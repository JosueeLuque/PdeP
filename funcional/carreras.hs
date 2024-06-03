import Text.Show.Functions()

-- Punto 1 --

data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distancia :: Int -- distancia que recorrio
} deriving (Show, Eq)

type Carrera = [Auto]

-- 1.a

estanCerca :: Auto -> Auto -> Bool
estanCerca auto1 auto2 = (auto1 /= auto2) && distanciaEntreAutos auto1 auto2 < 10

distanciaEntreAutos :: Auto -> Auto -> Int
distanciaEntreAutos auto1 = abs . (distancia auto1 -) . distancia

-- 1.b

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo unAuto carrera = all (not . estanCerca unAuto) carrera && all (recorrioMasDistancia unAuto) carrera

recorrioMasDistancia :: Auto -> Auto -> Bool
recorrioMasDistancia unAuto = (< distancia unAuto) . distancia

-- 1.c

puesto :: Auto -> Carrera -> Int
puesto unAuto carrera = (+1) . length . filter (not . recorrioMasDistancia unAuto) $ carrera


-- Punto 2

-- 2.a
corra :: Int -> Auto -> Auto
corra unTiempo unAuto = cambiarDistancia unAuto . (* velocidad unAuto) . (+ unTiempo) . distancia $ unAuto

cambiarDistancia :: Auto -> Int -> Auto
cambiarDistancia unAuto unValor = unAuto {distancia = unValor}

-- 2.b

cambiarVelocidad :: (Int -> Int) -> Auto-> Auto
cambiarVelocidad modificador unAuto = unAuto {velocidad = modificador (velocidad unAuto)}


bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad unValor unAuto = cambiarVelocidad (restarVelocidad unValor) unAuto

restarVelocidad :: Int -> Int -> Int
restarVelocidad unValor = max 0 . flip (-) unValor

-- Punto 3

type Poder = Auto -> Carrera -> Carrera

terremoto :: Poder
terremoto unAuto carrera = map (bajarVelocidad 50) . filter (estanCerca unAuto) $ carrera

terremoto' unAuto carrera = afectarALosQueCumplen (estanCerca unAuto) (bajarVelocidad 50) carrera

miguelitos :: Int -> Poder
miguelitos unaCantidad unAuto carrera = map (bajarVelocidad unaCantidad) . filter (recorrioMasDistancia unAuto) $ carrera

miguelitos' unaCantidad unAuto carrera = afectarALosQueCumplen (recorrioMasDistancia unAuto) (bajarVelocidad unaCantidad) carrera


{-
    jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
    Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
    Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo.
-}

jetPack :: Int -> Poder
-- jetPack unTiempo unAuto carrera = cambiarVelocidad (const velocidad unAuto) . corra unTiempo . cambiarVelocidad (2*) $ unAuto

jetPack unTiempo unAuto carrera = afectarALosQueCumplen (unAuto==) (activarJetPack unTiempo) carrera


activarJetPack :: Int -> Auto -> Auto
activarJetPack unTiempo unAuto = cambiarVelocidad (const (velocidad unAuto)) . corra unTiempo . cambiarVelocidad (2*) $ unAuto

--Funcion aux
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

-- Punto 4

type Puesto = (Int, String)
type Evento = Carrera -> Carrera

simularCarrera :: Carrera -> [Evento] -> [Puesto]
simularCarrera carrera eventos = tablaDePosiciones (aplicarEventos carrera eventos)

aplicarEventos :: Carrera -> [Evento] -> Carrera
aplicarEventos carrera [] = carrera
aplicarEventos carrera (x:xs) = aplicarEventos (x carrera) xs

tablaDePosiciones :: Carrera -> [Puesto]
tablaDePosiciones carrera = map (\unAuto -> (puesto unAuto carrera, color unAuto)) carrera

-- Punto 5