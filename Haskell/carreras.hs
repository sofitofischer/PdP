{- Queremos armar un programa que nos permita simular unas fantásticas carreras de autos en las cuales cada vehículo avanza tan rápido como puede para consagrarse campeón, aprovechando del uso de algunos poderes especiales (o power ups) que encuentren a lo largo del trayecto para sacar ventaja por sobre los demás autos.

De cada auto conocemos su color (que nos servirá para identificarlo durante el desarrollo de la carrera), la velocidad a la que está yendo y la distancia que recorrió, ambos valores de tipo entero.

De la carrera sólo nos interesa el estado actual de los autos que están participando, lo cual nos permitirá analizar cómo viene cada uno, y posteriormente procesar aquellos eventos que se den en la carrera para determinar el resultado de la misma.

Teniendo en cuenta lo descrito anteriormente se pide resolver los siguientes puntos explicitando el tipo de cada función desarrollada y utilizando los conceptos aprendidos del Paradigma Funcional, poniendo especial énfasis en el uso de Composición, Aplicación Parcial y Orden Superior. -}

{- Declarar los tipos Auto y Carrera como consideres convenientes para representar la información indicada y definir funciones para resolver los siguientes problemas:
Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay entre ellos (en valor absoluto) es menor a 10.
Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va ganando a todos (por haber recorrido más distancia que los otros).
Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que le van ganando. -}

data Auto = UnAuto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show, Eq)

data Carrera = UnaCarrera {
    autos :: [Auto]
} deriving (Show, Eq)

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = auto1 /= auto2 && abs (distancia auto1 - distancia auto2) < 10

vaTranquilo :: Carrera -> Auto -> Bool
vaTranquilo carrera auto = not (any (estaCerca auto) (autos carrera)) && all (leGana auto) (autos carrera)

{-vaTranquilo' :: Auto -> [Auto] -> Bool
vaTranquilo' auto autos 
    | estaLejos auto autos && vaPrimero auto autos = True
    | otherwise = False 

estaLejos :: Auto -> [Auto] -> Bool
estaLejos auto autos = all (not.estaCerca auto) autos

vaPrimero :: Auto -> [Auto] -> Bool
vaPrimero auto autos = (maximum (map distancia autos)) < (distancia auto)-}

leGana :: Auto -> Auto -> Bool
leGana auto1 auto2 = distancia auto1 > distancia auto2

puesto :: Carrera -> Auto -> Int
puesto carrera auto = 1 + length (filter (>distancia auto) (map distancia (autos carrera)))

{-Desarrollar las funciones necesarias para manipular el estado de los autos para que sea posible:
Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo indicada, la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba recorrida + ese tiempo * la velocidad a la que estaba yendo.
A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo que su velocidad final sea la resultante de usar dicho modificador con su velocidad actual.
Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo que se le reste a la velocidad actual la cantidad indicada, y como mínimo quede en 0, ya que no es válido que un auto quede con velocidad negativa. -}

correr :: Int -> Auto -> Auto
correr tiempo auto = auto {distancia = distancia auto + tiempo * velocidad auto}

modificarVelocidad :: (Int -> Int) -> Auto -> Auto
modificarVelocidad modificador auto = auto {velocidad = modificador (velocidad auto)}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad auto = modificarVelocidad (max 0 . flip (-) cantidad) auto

{- Como se explicó inicialmente sobre las carreras que queremos simular, los autos que participan pueden gatillar poderes especiales a los que denominamos power ups.
Estos poderes son variados y tienen como objetivo impactar al estado general de la carrera, ya sea afectando al auto que lo gatilló y/o a sus contrincantes dependiendo de qué poder se trate.
Inicialmente queremos poder representar los siguientes power ups, pero debería ser fácil incorporar más power ups a futuro para enriquecer nuestro programa: -}

-- Disponemos de la siguiente función: 
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

{- terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su velocidad en 50.
miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la velocidad los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto que gatilló el power up les vaya ganando.
jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo. -}

terremoto :: PowerUp
terremoto auto carrera = carrera {autos = afectarALosQueCumplen (estaCerca auto) (bajarVelocidad 50) (autos carrera)}

miguelitos :: Int -> PowerUp
miguelitos cantidad auto carrera = carrera {autos = afectarALosQueCumplen (leGana auto) (bajarVelocidad cantidad) (autos carrera)}

jetPack :: Int -> PowerUp
jetPack tiempo auto carrera = carrera {autos = filter (/= auto) (autos carrera) ++ [jetPack' tiempo auto]}

jetPack' :: Int -> Auto -> Auto
jetPack' tiempo auto = volverAVelocidadInicial (correr tiempo (modificarVelocidad (*2) auto))

volverAVelocidadInicial :: Auto -> Auto
volverAVelocidadInicial auto = modificarVelocidad (flip div 2) auto

{- A partir de todo lo construido hasta ahora queremos finalmente simular una carrera, para lo cual se provee una lista de eventos, que son funciones que permiten ir de un estado de la carrera al siguiente, y el estado inicial de la carrera a partir del cual se producen dichos eventos. Con esta información buscamos generar una “tabla de posiciones”, que incluye la información de en qué puesto quedó cada auto asociado al color del auto en cuestión.
Desarrollar la función:
simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
que permita obtener la tabla de posiciones a partir del estado final de la carrera, el cual se obtiene produciendo cada evento uno detrás del otro, partiendo del estado de la carrera recibido. -}

type Color = String

simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)]
simularCarrera carrera eventos = tablaDePosiciones (foldl (flip ($)) carrera eventos)

tablaDePosiciones :: Carrera -> [(Int, Color)]
tablaDePosiciones carrera = zip [1..] (map color (autos carrera))

{- Desarrollar las siguientes funciones de modo que puedan usarse para generar los eventos que se dan en una carrera:
correnTodos que hace que todos los autos que están participando de la carrera corran durante un tiempo indicado.
usaPowerUp que a partir de un power up y del color del auto que gatilló el poder en cuestión, encuentre el auto correspondiente dentro del estado actual de la carrera para usarlo y produzca los efectos esperados para ese power up. -}

correnTodos :: Int -> Carrera -> Carrera
correnTodos tiempo carrera = carrera {autos = map (correr tiempo) (autos carrera)}

type PowerUp = Auto -> Carrera -> Carrera

{-usaPowerUp :: PowerUp -> Color -> Carrera -> Carrera
usaPowerUp powerUp color carrera = powerUp (autoDeColor color carrera) carrera

autoDeColor :: Color -> Carrera -> Auto-}



{- Mostrar un ejemplo de uso de la función simularCarrera con autos de colores rojo, blanco, azul y negro que vayan inicialmente a velocidad 120 y su distancia recorrida sea 0, de modo que ocurran los siguientes eventos:
- todos los autos corren durante 30 segundos
- el azul usa el power up de jet pack por 3 segundos
- el blanco usa el power up de terremoto
- todos los autos corren durante 40 segundos
- el blanco usa el power up de miguelitos que reducen la velocidad en 20
- el negro usa el power up de jet pack por 6 segundos
- todos los autos corren durante 10 segundos -}

-- Ejemplos
rojo, blanco, azul, negro :: Auto
rojo = UnAuto "rojo" 120 0
blanco = UnAuto "blanco" 120 0
azul = UnAuto "azul" 120 0
negro= UnAuto "negro" 120 0

carrera :: Carrera
carrera = UnaCarrera [rojo, blanco, azul, negro]


