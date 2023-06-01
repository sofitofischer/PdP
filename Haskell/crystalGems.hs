{- Las Gemas son una especie de raza extraterrestre de creación artificial que se caracteriza por el culto a la tecnología y poseer habilidades sobrehumanas, con las cuales dominan planetas para expandir su imperio intergaláctico. Las Gemas tienen numerosas capacidades, entre ellas fusionarse entre sí para crear Gemas más fuertes.

Si bien existen varias Gemas de la misma naturaleza (ya que son construidas a partir de la misma piedra preciosa, que les da el nombre que las identifica), cada una puede tener distinta personalidad, que es lo que lleva a que una Gema se comporte de cierta forma ante una situación.

Queremos modelar cómo se comportan estos seres para que un grupo de Gemas rebeldes (las Gemas de Cristal) que intentan proteger la Tierra tengan mejores chances de vencer ante una invasión.

Tenemos las siguientes definiciones sobre las cuales partir: -}

data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)

type Situacion = [Aspecto]
-- tensión, incertidumbre y peligro

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor

mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto -> Situacion -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo :: String -> Situacion -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto :: Aspecto -> Situacion -> Situacion
reemplazarAspecto aspectoBuscado situacion =
    aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)

{- Sabemos que las situaciones están conformadas por un grupo acotado de aspectos problemáticos bien conocidos: tensión, incertidumbre y peligro; eventualmente podrían incorporarse más aspectos, pero se espera que cada situación incluya el grado correspondiente para cada uno de ellos. El orden en el que los mismos se encuentran al armar una situación es irrelevante.

1) Trabajando con Situaciones: 
Definir modificarAspecto que dada una función de tipo (Float -> Float) y un aspecto, modifique el aspecto alterando su grado en base a la función dada.
Saber si una situación es mejor que otra: esto ocurre cuando, para la primer situación, cada uno de los aspectos, es mejor que ese mismo aspecto en la segunda situación.
Nota: recordar que los aspectos no necesariamente se encuentran en el mismo orden para ambas situaciones. Sin embargo, las situaciones a comparar siempre tienen los mismos aspectos.
Definir una función modificarSituacion que a partir de una situación permita obtener otra de modo que se modifique de cierta forma el grado correspondiente a un tipo de aspecto buscado. La alteración a realizar sobre el grado actual de ese aspecto debe poder ser indicada al usar la función. -}

modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto
modificarAspecto funcion aspecto = aspecto { grado = funcion (grado aspecto) }

esMejorSituacion :: Situacion -> Situacion -> Bool
esMejorSituacion situacion1 situacion2 = all (uncurry mejorAspecto) (zip situacion1 situacion2)

modificarSituacion :: String -> (Float -> Float) -> Situacion -> Situacion
modificarSituacion tipoDeAspecto funcion situacion = reemplazarAspecto (modificarAspecto funcion (buscarAspectoDeTipo tipoDeAspecto situacion)) situacion

{- 2) Modelando Gemas y personalidades posibles: 
Modelar a las Gemas de modo que estén compuestas por su nombre, la fuerza que tienen y la personalidad. La personalidad de una Gema debe representar cómo reacciona ante una situación, derivando de ese modo a una situación diferente.
Definir las siguientes personalidades:
vidente: ante una situación disminuye a la mitad la incertidumbre y baja en 10 la tensión.
relajada: disminuye en 30 la tensión de la situación y, dependiendo de qué tan relajada sea la Gema, aumenta el peligro en tantas unidades como nivel de relajamiento tenga.
Mostrar ejemplos de cómo se crean una Gema vidente y una Gema descuidada. -}

data Gema = UnaGema {
  nombre :: String,
  fuerza :: Float,
  personalidad :: Personalidad
} --deriving (Show, Eq)

type Personalidad = Situacion -> Situacion

vidente :: Personalidad
vidente situacion = modificarSituacion "incertidumbre" (/2) . modificarSituacion "tension" (subtract 10) $ situacion

relajada :: Float -> Personalidad
relajada nivelRelajamiento situacion = modificarSituacion "tension" (subtract 30) . modificarSituacion "peligro" (+nivelRelajamiento) $ situacion

gemaVidente :: Gema
gemaVidente = UnaGema "Gema Vidente" 100 vidente

gemaDescuidada :: Gema
gemaDescuidada = UnaGema "Gema Descuidada" 100 (relajada 10)

-- 3) Saber si una Gema le gana a otra dada una situación, que se cumple si la primera es más o igual de fuerte que la segunda y además entre las dos personalidades, la situación resultante de la primera ante la situación dada es mejor que la que genera la segunda personalidad ante la misma situación.

leGana :: Gema -> Gema -> Situacion -> Bool
leGana gema1 gema2 situacion = (fuerza gema1 >= fuerza gema2) && (esMejorSituacion (personalidad gema1 situacion) (personalidad gema2 situacion))

{- 4) Fusión: como dijimos antes, dos Gemas pueden fusionarse entre ellas. La fusión de dos Gemas en una determinada situación produce una Gema enteramente nueva que cumple con las siguientes pautas:
Su nombre o bien es el mismo de las Gemas que se fusionaron si las mismas se llaman igual, o bien es la concatenación de los nombres de dichas Gemas.
Su personalidad fusionada va a producir el mismo efecto que producirían las gemas individuales actuando en sucesión, luego de bajar en 10 todos los aspectos de la situación a la que deban enfrentarse.
Para saber cómo va a ser la fuerza de la fusión necesitamos saber si son compatibles entre ellas, lo cual se cumple si, para la situación ante la cual se están fusionando, la personalidad fusionada produce una mejor situación que las personalidades individuales de cada gema. 
Si son compatibles, la fuerza de la fusión va a ser la suma de la fuerza de las gemas individuales multiplicada 10.
En caso contrario, su fuerza es 7 veces la fuerza de la gema dominante (si la primera le gana a la otra es la dominante, sino es la segunda). -}

fusion :: Gema -> Gema -> Situacion -> Gema
fusion gema1 gema2 situacion
  | sonCompatibles gema1 gema2 situacion = UnaGema (nombre gema1 ++ nombre gema2) (fuerza gema1 + fuerza gema2 * 10) (fusionarPersonalidades gema1 gema2 situacion)
  | otherwise = UnaGema (nombre gemaDominante) (fuerza gemaDominante * 7) (fusionarPersonalidades gema1 gema2 situacion)
  where gemaDominante = gemaMasFuerte gema1 gema2 situacion

sonCompatibles :: Gema -> Gema -> Situacion -> Bool
sonCompatibles gema1 gema2 situacion = esMejorSituacion (fusionarPersonalidades gema1 gema2 situacion) (personalidad gema1 situacion) && esMejorSituacion (fusionarPersonalidades gema1 gema2 situacion) (personalidad gema2 situacion)

fusionarPersonalidades :: Gema -> Gema -> Situacion -> Personalidad
fusionarPersonalidades gema1 gema2 situacion = personalidad gema1 . personalidad gema2 . bajarAspectos $ situacion

bajarAspectos :: Situacion -> Situacion
bajarAspectos = map (modificarAspecto (subtract 10))

gemaMasFuerte :: Gema -> Gema -> Situacion -> Gema
gemaMasFuerte gema1 gema2 situacion
  | fuerza gema1 >= fuerza gema2 = gema1
  | otherwise = gema2

asignarNombre :: Gema -> Gema -> Gema
asignarNombre gema1 gema2
  | nombre gema1 == nombre gema2 = UnaGema {nombre = nombre gema1}
  | otherwise = UnaGema {nombre = nombre gema1 ++ nombre gema2}

-- 5) Fusión grupal: las fusiones entre gemas no están limitadas a sólo dos de ellas, también pueden fusionarse un grupo de Gemas en caso de que sea necesario. Una fusión grupal es el resultado de fusionar a todas las Gemas entre sí hasta que quede sólo una.

{- 6) Tipado y estrategias de evaluación: 
Inferir el tipo de la función foo
Indicar para cada una de las invocaciones:
Si hay errores de tipos, justificando cuáles
Si tipa y termina, justificando por qué
Si tipa pero no termina, justificando por qué -}

foo x y z = any (== y x).z

-- Invocaciones de la función
foo 5 (+7) [1..]
foo 3 even (map (< 7))
foo 3 even [1, 2, 3]
foo [1..] head (take 5) [1.. ]




