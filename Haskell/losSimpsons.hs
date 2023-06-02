-- 1: Actividades de los personajes

data Personaje = Personaje {
    nombre :: String,
    dinero :: Int,
    felicidad :: Int
} deriving (Show, Eq)

-- Personajes
homero = Personaje "Homero" 100 100
lisa = Personaje "Lisa" 100 100
skinner = Personaje "Skinner" 100 100
burns = Personaje "Burns" 100000 100
bart = Personaje "Bart" 6 100

-- Actividades
type Actividad = Personaje -> Personaje 

irAEscuela :: Actividad
irAEscuela personaje
    | nombre personaje == "Lisa" = modificarFelicidad 20 personaje
    | otherwise = modificarFelicidad (-20) personaje

-- Ejemplo de consulta: irAEscuela homero -- Personaje {nombre = "Homero", dinero = 100, felicidad = 80}
-- Ejemplo 2 de consulta: irAEscuela lisa -- Personaje {nombre = "Lisa", dinero = 100, felicidad = 120}

comerDonas :: Int -> Actividad
comerDonas cantidad = modificarDinero (-10) . modificarFelicidad (10 * cantidad)

-- Ejemplo de consulta: comerDonas 12 homero -- Personaje {nombre = "Homero", dinero = 90, felicidad = 220}

irATrabajar :: String -> Actividad
irATrabajar trabajo = modificarDinero (length trabajo)

-- Ejemplo de consulta: irATrabajar "planta nuclear" homero -- Personaje {nombre = "Homero", dinero = 114, felicidad = 100}

irATrabajarComoDirector :: Actividad
irATrabajarComoDirector personaje = irATrabajar "escuela elemental" (modificarFelicidad (-20) personaje)

-- Ejemplo de consulta: irATrabajarComoDirector skinner -- Personaje {nombre = "Skinner", dinero = 117, felicidad = 80}

-- Actividad inventada (Ir a un bar): se le suma 50 a la felicidad. Si es lisa, se resta 50 a la felicidad.
irABar :: Actividad
irABar personaje
    | nombre personaje == "Lisa" = modificarFelicidad (-50) personaje
    | otherwise = modificarFelicidad 50 personaje

-- Ejemplo de consulta: irABar lisa -- Personaje {nombre = "Lisa", dinero = 100, felicidad = 50}
-- Ejemplo 2 de consulta: irABar homero -- Personaje {nombre = "Homero", dinero = 100, felicidad = 150}

modificarFelicidad :: Int -> Actividad
modificarFelicidad cantidad personaje = personaje {felicidad = max 0 (felicidad personaje + cantidad)}

modificarDinero ::  Int -> Actividad
modificarDinero cantidad personaje = personaje {dinero = dinero personaje + cantidad}

realizarActividades :: [Actividad] -> Personaje -> Personaje
realizarActividades actividades personaje = foldl (flip ($)) personaje actividades

-- Ejemplo de consulta: realizarActividades [irAEscuela, irABar] lisa -- Personaje {nombre = "Lisa", dinero = 100, felicidad = 70}

-- 2: Logros

type Logro = Personaje -> Bool

serMillonario :: Logro
serMillonario personaje = dinero personaje > dinero burns

alegrarse :: Int -> Logro
alegrarse nivel personaje = felicidad personaje > nivel

irAVerAKrosty :: Logro
irAVerAKrosty personaje = dinero personaje >= 10

-- Logro inventado (Ser feliz): la felicidad del personaje debe ser mayor a 150 y debe tener más de $120.
serFeliz :: Logro
serFeliz personaje = felicidad personaje > 150 && dinero personaje > 120

-- A)
esDecisiva :: Actividad -> Logro -> Personaje -> Bool
esDecisiva actividad logro personaje = not(logro personaje) && logro (actividad personaje)

-- Ejemplo de consulta: esDecisiva (irATrabajar "mafia") irAVerAKrosty bart -- True

-- B)
primeraActividadDecisiva :: Personaje -> Logro -> [Actividad] -> Personaje
primeraActividadDecisiva personaje logro actividades
    | null actividades = personaje
    | esDecisiva (head actividades) logro personaje = (head actividades) personaje
    | otherwise = primeraActividadDecisiva personaje logro (tail actividades)

-- Ejemplo de consulta: primeraActividadDecisiva bart irAVerAKrosty [irATrabajar "no", irATrabajar "mafia"] -- Personaje {nombre = "Bart", dinero = 11, felicidad = 100}
-- Ejemplo 2 de consulta: primeraActividadDecisiva homero serMillonario [irATrabajar "mafia", irATrabajar "planta nuclear"] -- Personaje {nombre = "Homero", dinero = 100, felicidad = 100}
-- Ejemplo 3 de consulta: primeraActividadDecisiva lisa (alegrarse 105) [irATrabajar "mafia", irAEscuela] -- Personaje {nombre = "Lisa", dinero = 100, felicidad = 120}

-- C)
listaInfinitaActividades :: [Actividad]
listaInfinitaActividades = irAEscuela : listaInfinitaActividades

-- Ejemplos de consulta:
-- primeraActividadDecisiva lisa (alegrarse 105) listaInfinitaActividades -- Personaje {nombre = "Lisa", dinero = 100, felicidad = 120}
-- primeraActividadDecisiva bart irAVerAKrosty listaInfinitaActividades -- no termina nunca

-- Justificación conceptual: 
-- En el primer ejemplo, la primera actividad decisiva es ir a la escuela, por lo que la felicidad de Lisa aumenta en 20 y la función devuelve el personaje con esos cambios. En el segundo ejemplo, nunca se cumple la condición de que la actividad sea decisiva, por lo que la función nunca termina de ejecutarse.