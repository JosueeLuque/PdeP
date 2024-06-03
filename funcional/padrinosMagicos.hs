import Text.Show.Functions()

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
} deriving (Show)

type Habilidad = String

type Deseo = Chico -> Chico


-- Punto A - Concediendo deseos --

-- A.1
aprenderHabilidades :: [Habilidad] -> Chico -> Chico
aprenderHabilidades habilidades unChico = agregarHabilidades habilidades unChico

agregarHabilidades :: [Habilidad] -> Chico -> Chico
agregarHabilidades habilidades unChico = cambiarHabilidades (habilidades ++) unChico

cambiarHabilidades :: ([Habilidad] -> [Habilidad]) -> Chico -> Chico
cambiarHabilidades modificador unChico = unChico {habilidades = modificador (habilidades unChico)}

serGrosoEnNeedForSpeed :: Chico -> Chico
serGrosoEnNeedForSpeed unChico = cambiarHabilidades (agregarJuegos ++) unChico

agregarJuegos :: [Habilidad]
agregarJuegos = map (\unNumero -> "jugar need for speed " ++ show unNumero) [1..]


serMayor :: Chico -> Chico
serMayor unChico = cambiarEdad 18 unChico

cambiarEdad :: Int -> Chico -> Chico
cambiarEdad unaEdad unChico = unChico {edad = unaEdad}

-- A.2
wanda :: Chico -> Chico
wanda unChico = modificarMadurez (1+) . cumplirDeseo unChico . head . deseos $ unChico 

cosmo :: Chico -> Chico
cosmo unChico = modificarMadurez (flip div 2) $ unChico

muffinMagico :: Chico -> [Chico]
muffinMagico unChico = map (cumplirDeseo unChico) (deseos unChico)


cumplirDeseo :: Chico -> (Deseo) -> Chico
cumplirDeseo unChico unDeseo = unDeseo unChico 

modificarMadurez :: (Int -> Int) -> Chico -> Chico
modificarMadurez f unChico = cambiarEdad (f (edad unChico)) unChico


-- Punto B - En busqueda de pareja --

-- B.1
tieneHabilidad :: Habilidad -> Chico -> Bool
tieneHabilidad unaHabilidad = (elem unaHabilidad) . habilidades

esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = esMayorDeEdad unChico && tieneHabilidad "manejar" unChico

esMayorDeEdad :: Chico -> Bool
esMayorDeEdad = (>18) . edad

-- B.2
noesTimmy :: Condicion
noesTimmy = (/= "Timmy") . nombre 

data Chica = Chica {
    nombreChica :: String,
    condicion :: Condicion
}

type Condicion = Chico -> Bool

timmy :: Chico
timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

mario::Chico
mario= Chico "Mario" 15 ["ser violinista", "ser un super modelo","dominar El Mundo", "enamorar"] [serMayor]

trixie::Chica
trixie = Chica "Trixie Tang" noesTimmy

vicky :: Chica
vicky = Chica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

-- B.a
quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica losPretendientes = foldr cumpleCondicion (last losPretendientes) losPretendientes
  where
    cumpleCondicion pretendiente acumulador
      | condicion unaChica pretendiente = pretendiente

-- B.b
dana::Chica 
dana = Chica "Carolina" (tieneHabilidad "saber cocinar")


-- Punto C - Da Rules --
infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules chicos = nombreDeInfractores . filter tieneDeseoProhibido $ chicos

tieneDeseoProhibido :: Chico -> Bool
tieneDeseoProhibido unChico = any (esDeseoProhibido unChico) . deseos $ unChico

esDeseoProhibido :: Chico -> (Deseo) -> Bool
esDeseoProhibido unChico unDeseo = tieneHabilidadProhibida . unDeseo $ unChico

tieneHabilidadProhibida :: Chico -> Bool
tieneHabilidadProhibida unChico = any (esHabilidadProhibida) . take 5 . habilidades $ unChico

esHabilidadProhibida :: Habilidad -> Bool
esHabilidadProhibida unaHabilidad = elem unaHabilidad habilidadesProhibidas

habilidadesProhibidas :: [Habilidad]
habilidadesProhibidas = ["enamorar", "matar", "dominar el mundo"]

nombreDeInfractores :: [Chico] -> [String]
nombreDeInfractores chicos = map nombre chicos


{-
  listas infinitas 

  infraforesDeDaRules ([timmy] ++ repeat mario)

  --> Ira mostrando una lista repetida con el nombre de Mario, ya que es el que tiene habilidades prohibidas
-}