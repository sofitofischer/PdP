data Turista = UnTurista {
    cansancio :: Int,
    estres :: Int,
    viajaSolo :: Bool,
    idiomas :: [String]
} deriving (Show)

{-La isla contiene varias excursiones para los turistas, por ahora nos pidieron modelar estas:

Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 
Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
Paseo en barco: depende de cómo esté la marea
si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
si está moderada, no pasa nada.
si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes. -}

type Excursion = Turista -> Turista

irALaPlaya :: Excursion
irALaPlaya turista 
    | viajaSolo turista = cambiarCansancio (-5) turista
    | otherwise = cambiarEstres (-1) turista

apreciarElemento :: String -> Excursion
apreciarElemento n = cambiarEstres (-length n)

salirAHablar :: String -> Excursion
salirAHablar n = viajaAcompaniado . aprenderIdioma n

caminar :: Int -> Excursion
caminar n = cambiarEstres (div (-n) 4) . cambiarCansancio (div n 4)

paseoEnBarco :: String -> Excursion
paseoEnBarco n turista
    | n == "fuerte" = (cambiarEstres 6 . cambiarCansancio 10) turista
    | n == "moderada" = turista
    | n == "tranquila" = (apreciarElemento "mar" . salirAHablar "aleman" . caminar 10) turista

cambiarEstres :: Int -> Excursion
cambiarEstres n turista = UnTurista (cansancio turista) (estres turista + n) (viajaSolo turista) (idiomas turista)

cambiarCansancio :: Int -> Excursion
cambiarCansancio n turista = UnTurista (cansancio turista + n) (estres turista) (viajaSolo turista) (idiomas turista)

aprenderIdioma :: String -> Excursion
aprenderIdioma n turista 
    | n `elem` idiomas turista = turista    
    | otherwise = UnTurista (cansancio turista) (estres turista) (viajaSolo turista) (n : idiomas turista)

viajaAcompaniado :: Excursion
viajaAcompaniado turista = turista {viajaSolo = False}

{-Nos avisaron que es común que, cada cierto tiempo, se vayan actualizando las excursiones que ofrecen, en base a las nuevas demandas que surgen en el mercado turístico. 
Se pide
Crear un modelo para los turistas y crear los siguientes tres ejemplos:
Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress. -}

ana, beto, cathi :: Turista
ana = UnTurista 0 21 False ["espaniol"]
beto = UnTurista 15 15 True ["aleman"]
cathi = UnTurista 15 15 True ["aleman", "catalan"]

{-Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga falta modificar las funciones existentes. Además:

Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.

Dada la función:
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2
Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
Por ejemplo, si “stress” es la función que me da el stress de un turista:
> deltaExcursionSegun stress ana irALaPlaya
-3     -- porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20) 

Usar la función anterior para resolver cada uno de estos puntos:
Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista. -}

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = (cambiarEstres (-div (estres turista) 10) . excursion) turista

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun f turista excursion = deltaSegun f (hacerExcursion excursion turista) turista

esEducativa :: Excursion -> Turista -> Bool
esEducativa excursion turista = (length . idiomas . hacerExcursion excursion) turista > (length . idiomas) turista

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter ((<= -3) . deltaExcursionSegun estres turista)

{-Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones llamados tours. Un tour se compone por una serie de excursiones.
Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, y finaliza con una salida con gente local que habla "melmacquiano".
Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra isla: si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino. -}

type Tour = [Excursion]

completo :: Tour
completo = [caminar 20, apreciarElemento "cascada", caminar 40, irALaPlaya, salirAHablar "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco "tranquila", excursion, caminar 120]

islaVecina :: String -> Tour
islaVecina marea = [paseoEnBarco marea, excursionSegunMarea marea, paseoEnBarco marea]

excursionSegunMarea :: String -> Excursion
excursionSegunMarea marea
    | marea == "fuerte" = apreciarElemento "lago"
    | otherwise = irALaPlaya

{-Modelar los tours para:
Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades como cantidad de excursiones tenga el tour, y luego realizar las excursiones en orden.
Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla.
Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista a quienes les resultó convincente el tour. 
La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour. -}

hacerTour :: Tour -> Turista -> Turista
hacerTour tour turista = foldl (flip hacerExcursion) (cambiarEstres (length tour) turista) tour

tourConvincente :: Turista -> [Tour] -> Bool
tourConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (quedaAcompaniado turista) . excursionesDesestresantes turista

quedaAcompaniado :: Turista -> Excursion -> Bool
quedaAcompaniado turista = not . viajaSolo . flip hacerExcursion turista

efectividad :: Tour -> [Turista] -> Int
efectividad tour = sum . map (espiritualidad tour) . filter (`esConvincente` tour)

espiritualidad :: Tour -> Turista -> Int
espiritualidad tour turista = deltaSegun estres turista (hacerTour tour turista) + deltaSegun cansancio turista (hacerTour tour turista)

{-Implementar y contestar en modo de comentarios o pruebas por consola
Construir un tour donde se visiten infinitas playas.
¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar. -}

tourInfinito :: Tour
tourInfinito = repeat irALaPlaya

-- No se puede saber si es convincente para Ana porque no termina de hacer el tour, pero sí se puede saber para Beto porque sí termina de hacerlo.

-- No se puede conocer la efectividad de este tour porque no termina de hacerlo.

{- rta: Para Ana sí porque la primera actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0. -}