import Text.Show.Functions

--PUNTO 1
data Heroe = Heroe {
    epiteto :: String,
    reconocimiento :: Int,
    artefactos :: [Artefacto],
    tareas :: [Tarea] 
} deriving (Show)

data Artefacto = Artefacto {
    nombre :: String,
    rareza :: Int
} deriving (Show)

type Tarea = Heroe -> Heroe

--PUNTO 2
pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unHeroe
    | reconocimiento unHeroe > 1000 = cambiarEpiteto "El mítico" unHeroe
    | reconocimiento unHeroe >= 500 = cambiarEpiteto "El magnifico" . agregarArtefacto lanzaDeOlimpo $ unHeroe
    | reconocimiento unHeroe > 100 = cambiarEpiteto "Hoplita" . agregarArtefacto xiphos $ unHeroe


cambiarEpiteto :: String -> Heroe -> Heroe
cambiarEpiteto unEpiteto unHeroe = unHeroe {epiteto = unEpiteto}

agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unArtefacto = cambiarArtefactos (unArtefacto :)

cambiarArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
cambiarArtefactos modificador unHeroe = unHeroe {artefactos = modificador (artefactos unHeroe)}

lanzaDeOlimpo :: Artefacto
lanzaDeOlimpo = Artefacto "Lanza del Olimpo" 100

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50

relampagoDeZeus :: Artefacto
relampagoDeZeus = Artefacto "El relámago de Zeus" 500

--PUNTO 3
encontrarUnArtefacto :: Artefacto -> Tarea
encontrarUnArtefacto unArtefacto = ganaReconocimiento (rareza unArtefacto) . agregarArtefacto unArtefacto

ganaReconocimiento :: Int -> Heroe -> Heroe
ganaReconocimiento cantidad unHeroe = unHeroe {reconocimiento = cantidad + reconocimiento unHeroe}

escalarElOlimpo :: Tarea
escalarElOlimpo unHeroe = agregarArtefacto relampagoDeZeus . desecharArtefactosComunes . triplicarRarezaArtefactos . ganaReconocimiento 500 $ unHeroe 

triplicarRarezaArtefactos :: Tarea
triplicarRarezaArtefactos = cambiarArtefactos (map triplicarRarezaArtefacto)

triplicarRarezaArtefacto :: Artefacto -> Artefacto
triplicarRarezaArtefacto unArtefacto = unArtefacto {rareza = (*3) . rareza $ unArtefacto}

desecharArtefactosComunes :: Tarea
--desecharArtefactosComunes = unHeroe {artefactos = filter (not . esComun) (artefactos unHeroe)}
desecharArtefactosComunes = cambiarArtefactos (filter (not . esComun))

esComun :: Artefacto -> Bool
esComun unArtefacto = rareza unArtefacto < 1000
esComun' :: Artefacto -> Bool
esComun' = (<1000) . rareza



ayudarACruzarLaCalle :: Int -> Tarea
ayudarACruzarLaCalle cantidadDeCuadras = cambiarEpiteto ("Gros" ++ replicate cantidadDeCuadras 'o')

matarUnaBestia :: Bestia -> Tarea 
matarUnaBestia unaBestia unHeroe
    | (debilidad unaBestia) unHeroe = cambiarEpiteto ("El asesino de " ++ nombreBestia unaBestia) $ unHeroe
    | otherwise = (cambiarEpiteto "El cobarde") . (cambiarArtefactos (drop 1)) $ unHeroe

data Bestia = Bestia {
    nombreBestia :: String,
    debilidad :: Debilidad
} deriving (Show)

type Debilidad = Heroe -> Bool

--PUTNO 4
heracles :: Heroe
heracles = Heroe "Guardían del Olimpo" 700 [pistolaRara, relampagoDeZeus] [matarAlLeonDeNemea]

pistolaRara :: Artefacto
pistolaRara = Artefacto "Fierro de la antigua Grecia" 1000

--PUNTO 5
matarAlLeonDeNemea :: Tarea
matarAlLeonDeNemea = matarUnaBestia leonDeNemea

leonDeNemea :: Bestia
leonDeNemea = Bestia "León de Nemea" ((>20) . length . epiteto)

--PUNTO 6
hacerUnaTarea :: Tarea -> Heroe -> Heroe
hacerUnaTarea unaTarea = agregarTarea unaTarea . unaTarea 

agregarTarea :: Tarea -> Heroe -> Heroe
agregarTarea unaTarea unHeroe = unHeroe {tareas = unaTarea : tareas unHeroe}

--PUNTO 7
-- presumir :: Heroe -> Heroe -> (Heroe , Heroe)
-- presumir unHeroe otroHeroe
--     | reconocimiento unHeroe > reconocimiento otroHeroe = (unHeroe , otroHeroe)
--     | reconocimiento unHeroe < reconocimiento otroHeroe = (otroHeroe , unHeroe)
--     | sumatoriaRarezas unHeroe > sumatoriaRarezas otroHeroe = (unHeroe , otroHeroe)
--     | sumatoriaRarezas otroHeroe > sumatoriaRarezas otroHeroe = (otroHeroe , unHeroe)
--     | otherwise = presumir (realizarLabor (tareas otroHeroe) unHeroe) (realizarLabor (tareas unHeroe) otroHeroe)
presumir :: Heroe -> Heroe -> (Heroe , Heroe)
presumir heroe1 heroe2
    | gana heroe1 heroe2 = (heroe1 , heroe2)
    | gana heroe2 heroe1 = (heroe2 , heroe1)
    | otherwise = presumir (realizarTareasDe heroe1 heroe2) (realizarTareasDe heroe1 heroe2)

realizarTareasDe :: Heroe -> Heroe -> Heroe
realizarTareasDe unHeroe otroHeroe = realizarLabor (tareas otroHeroe) unHeroe

gana :: Heroe -> Heroe -> Bool
gana ganador perdedor = reconocimiento ganador > reconocimiento perdedor || reconocimiento ganador == reconocimiento perdedor && sumatoriaRarezas ganador > sumatoriaRarezas perdedor

sumatoriaRarezas :: Heroe -> Int
sumatoriaRarezas = sum . map rareza . artefactos

--PUNTO 8
--Nada, se queda pensando y no llega nunca a nada

--PUNTO 9
realizarLabor :: [Tarea] -> Heroe -> Heroe
realizarLabor unasTareas unHeroe = foldl (flip hacerUnaTarea) unHeroe unasTareas

--PUNTO 10
--no, no se podrá conocer el estado final porque se iterará infinitamente y nunca podrá mostrarse un "estado final"