{- Las leyes:
El Congreso de la Nación sanciona diferentes leyes cada año, algunas más extensas o importantes que otras, pero en particular más que el detalle de su articulado nos interesa conocer cierta información clave, como ser el tema que trata, el presupuesto que requiere su implementación y cuáles son los partidos políticos, grupos de poder u otros sectores que la impulsaron o la apoyan. Por ejemplo:

La ley de uso medicinal del cannabis implica un presupuesto de 5 unidades, fue apoyada por el partido cambio de todos y el sector financiero.
La ley de educación superior requiere un presupuesto de 30 unidades y fue apoyada por docentes universitarios y el partido de centro federal
A la ley de profesionalización del tenista de mesa, con un presupuesto de 1 unidad la apoya el partido de centro federal, la liga de deportistas autónomos y el club paleta veloz.
También hay una ley sobre tenis, apoyada por la liga de deportistas autónomos, con un presupuesto de 2. -}

data Ley = UnaLey {
    tema :: String,
    presupuesto :: Int,
    sectores :: [String]
} deriving (Show, Eq)

leyCannabis = UnaLey "uso medicinal del cannabis" 5 ["cambio de todos", "sector financiero"]
leyEducacion = UnaLey "educación superior" 30 ["docentes universitarios", "centro federal"]
leyProfesionalizacion = UnaLey "profesionalización del tenista de mesa" 1 ["centro federal", "liga de deportistas autónomos", "club paleta veloz"]
leyTenis = UnaLey "tenis" 2 ["liga de deportistas autónomos"]

-- 1) Averiguar si dos leyes son compatibles, que se da cuando tienen al menos un sector en común que la apoya y el tema de una de las leyes está incluido en el otro. Por ejemplo, son compatibles la ley de “profesionalización del tenista de mesa” y la de “tenis”. -}

hayElementoEnComun :: [String] -> [String] -> Bool
hayElementoEnComun lista1 lista2 = any (`elem` lista1) lista2

substring :: String -> String -> Bool
substring string1 "" = False 
substring palabra1 (letra2:palabra2) = palabra1 == (take (length palabra1) palabra2) || substring palabra1 palabra2 

sonCompatibles :: Ley -> Ley -> Bool
sonCompatibles ley1 ley2 = sectoresEnComun ley1 ley2 && mismosTemas ley1 ley2

sectoresEnComun :: Ley -> Ley -> Bool
sectoresEnComun ley1 ley2 = hayElementoEnComun (sectores ley1) (sectores ley2)

mismosTemas :: Ley -> Ley -> Bool
mismosTemas ley1 ley2 = substring (tema ley1) (tema ley2  ) || substring (tema ley2) (tema ley1)

{- Constitucionalidad de las leyes:
La legislación vigente establece que son 5 los jueces que integran la Corte Suprema, pero previendo posibles cambios que puedan suceder contemplaremos la posibilidad de que la cantidad de integrantes sea diferente. Es tarea de la corte establecer si una determinada ley es constitucional o no. Para ello, cada juez vota de acuerdo a sus principios, experiencia, intereses o como le dé la gana, y si resultan mayoritarios los votos negativos, la ley se declara inconstitucional. En estos casos, ningún juez de la corte puede abstenerse o votar en blanco.

De los jueces no nos interesa saber su información personal, su patrimonio ni su autopercepción, sino simplemente cómo votan.
Algunos de ellos son:
Uno de los jueces se basa en la opinión pública: si el tema de la ley está en agenda, seguro que la declara constitucional. (Se conoce el conjunto de temas en agenda, que es único para todos)
Otro de los jueces, cuando se entera que la ley fue apoyada por el sector financiero, es imposible que la declare inconstitucional.
Hay un juez muy preocupado por las arcas del estado que declara inconstitucionales las leyes que requieren un presupuesto de más de 10 unidades. 
Existe otro juez con mentalidad similar pero un poco más tolerante que las declara inconstitucional recién si superan las 20 unidades de presupuesto.
Y el último de los jueces actuales decide declarar constitucional a toda ley que haya sido apoyada por el partido conservador -}

type Juez = Ley -> Bool 

agenda = ["uso medicinal del cannabis", "educación superior", "profesionalización del tenista de mesa", "tenis"]

juezOpinionPublica :: Juez
juezOpinionPublica ley = elem (tema ley) agenda

juezSector :: Juez
juezSector ley = elem "sector financiero" (sectores ley)

juezPresupuesto10 :: Juez
juezPresupuesto10 ley = juezPresupuesto 10 ley

juezPresupuesto20 :: Juez
juezPresupuesto20 ley = juezPresupuesto 20 ley

juezConservador :: Juez
juezConservador ley = elem "conservador" (sectores ley)

{- 2) Hacer que una Corte Suprema determine si se considera constitucional o no una ley. Agregar nuevos jueces que puedan integran la corte suprema:
Uno que siempre vote afirmativamente
Un juez inventado, con lógica totalmente diferente (no trivial).
Otro juez que también tenga preocupación presupuestaria pero con otro importe. Hacer una función que dada una serie de leyes, averigue cuáles que no serían consideradas constitucionales con la actual conformación de la corte sí lo serían en caso de agregarle un conjunto de nuevos integrantes. -}

juezPositivo :: Juez
juezPositivo ley = True

juezInventado :: Juez
juezInventado ley = juezSector ley && juezPresupuesto10 ley

juezPresupuesto :: Int -> Juez
juezPresupuesto presup ley = presupuesto ley <= presup

type CorteSuprema = [Juez]

corteOriginal = [juezOpinionPublica, juezSector, juezPresupuesto10, juezPresupuesto20, juezConservador]
juecesNuevos = [juezPositivo, juezInventado, juezPresupuesto 15]

votoCorteSuprema :: CorteSuprema -> Ley -> Bool
votoCorteSuprema corteSuprema ley = (length (filter (==True) (map ($ ley) corteOriginal))) > (length corteOriginal `div` 2)

-- Ejemplo de consulta: votoCorteSuprema corteOriginal leyCannabis

antesNoPeroAhoraSi :: CorteSuprema -> CorteSuprema -> Ley -> Bool
antesNoPeroAhoraSi corteSuprema juecesNuevos ley = not (votoCorteSuprema corteSuprema ley) && votoCorteSuprema (corteSuprema ++ juecesNuevos) ley

-- Ejemplo de consulta: antesNoPeroAhoraSi corteOriginal juecesNuevos leyCannabis

{- Cuestión de principios:
A veces pasa que a los jueces les pasan cosas, se sospecha de su independencia o de pronto cambian el sentido de su voto.

Hacer la función borocotizar, que dada una conformación de la Corte Suprema pasen a votar de forma contraria a lo que votaban antes y de esta manera, para cualquier ley, se cumpla que: constitucionalidad corteSuprema unaLey != constitucionalidad (borocotizar corteSuprema) unaLey

Determinar si un juez curiosamente coincide en su posición con un sector social, que se da cuando de un conjunto dado de leyes actualmente en tratamiento, sólo vota las que son apoyadas por dicho sector. -}

borocotizar :: CorteSuprema -> CorteSuprema
borocotizar corteSuprema = map (not .) corteSuprema

-- Ejemplo de consulta: votoCorteSuprema corteOriginal leyCannabis == votoCorteSuprema (borocotizar corteOriginal) leyCannabis

juezCoincideCon :: Juez -> String -> [Ley] -> Bool
juezCoincideCon juez sector leyes = all (sectorApoyaLey sector) (filter juez leyes)

sectorApoyaLey :: String -> Ley -> Bool
sectorApoyaLey sector ley = elem sector (sectores ley)

-- Ejemplo de consulta: juezCoincideCon juezOpinionPublica "centro federal" [leyEducacion, leyProfesionalizacion] -- True
