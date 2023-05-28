{- Se desea desarrollar un sistema para un popular gimnasio que permita calcular el efecto de las rutinas de ejercicios que realizan sus socios con las máquinas que dispone.
Las máquinas tienen ciertos ejercicios de fábrica (algunos son customizables), los cuales pueden
realizarse durante una cantidad de minutos determinada, y sirven para tonificar músculos y/o quemar calorías.
De cada persona nos interesa saber su edad, su peso y su coeficiente de tonificación. Tenemos algunos ejemplos de socios para realizar las pruebas:
­­(edad, peso, tonificacion)
pancho = ( 40, 120, 1)
andres = ( 22, 80, 6)
Los ejercicios que se pueden hacer son funciones que dada una persona y una cantidad de minutos,
retorna a la persona luego de realizar el ejercicio. Un ejemplo simple de ejercicio en el cual la persona no hace nada (y por ende queda igual que al principio sin importar cuánto tiempo lo realice) podría ser: -}
relax minutos persona = persona

type Ejercicio = Int -> Persona -> Persona

data Persona = UnaPersona {
    edad :: Int,
    peso :: Float,
    tonificacion :: Int
} deriving (Show, Eq)

-- Ejemplos
pancho, andres :: Persona
pancho = UnaPersona 40 120 1
andres = UnaPersona 22 80 6

{- Saber si alguien está saludable, lo cual se cumple si no está obeso y tiene una tonificación mayor a 5. Alguien es obeso si pesa más de 100 kilos.
> saludable pancho
False
> saludable andres
True -}

saludable :: Persona -> Bool
saludable persona = not (obeso persona) && tonificacion persona > 5

obeso :: Persona -> Bool
obeso = (>100) . peso

{- Hacer que la persona baje de peso en base a una cantidad de calorías quemadas
● Si la persona es obesa, baja 1 kilo cada 150 calorías quemadas.
● Si no es obesa pero tiene más de 30 años y las calorías quemadas son más de 200, baja
siempre un kilo.
● En cualquier otro caso se baja la cantidad de calorías quemadas dividido por el producto
entre el peso y la edad de la persona.
> quemarCalorias pancho 300
( 40, 118, 1)
> quemarCalorias andres 300
( 22, 79.4, 6) -}

quemarCalorias :: Persona -> Int -> Persona
quemarCalorias persona calorias
    | obeso persona = persona {peso = peso persona - fromIntegral(calorias `div` 150)}
    | edad persona > 30 && calorias > 200 = modificarPeso (-1) persona
    | otherwise = persona {peso = peso persona - (fromIntegral calorias / (fromIntegral(edad persona) * peso persona))}

modificarPeso :: Float -> Persona -> Persona
modificarPeso kilos persona = persona {peso = peso persona + kilos}

{- Desarrollar las funciones para los ejercicios caminata, entrenamientoEnCinta, pesas, colina y
montaña sabiendo que:
a. La cinta quema calorías en función de la velocidad promedio alcanzada durante el ejercicio, quemando 1 caloría por la velocidad promedio por minuto.
i. La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.
ii. El entrenamiento en cinta arranca en 6 km/h y cada 5 minutos incrementa la velocidad en 1 km/h, con lo cual la velocidad máxima dependerá de los minutos de entrenamiento.
b. Las pesas tonifican la décima parte de los kilos a levantar si se realiza por más de 10 minutos, sino nada.
c. La colina quema 2 calorías por minuto multiplicado por la inclinación de la colina.
d. La montaña son 2 colinas sucesivas (cada una con la mitad de duración respecto de los minutos totales indicados), donde la segunda colina tiene una inclinación de 3 más que la inclinación inicial elegida. Además de hacer perder peso por las calorías quemadas por las colinas, este ejercicio incrementa en una unidad la tonificación de la persona.
Resolver usando composición y aplicación parcial. -}

cinta :: Int -> Ejercicio
cinta velocidad minutos persona = quemarCalorias persona ((velocidad * minutos) `div` 60)

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos persona = cinta (((6*minutos) `div` 60) + (minutos `div` 60)) minutos persona

caminata :: Ejercicio
caminata = cinta 5

pesas :: Int -> Ejercicio
pesas kilos minutos persona
    | minutos > 10 = modificarTonificacion (kilos `div` 10) persona
    | otherwise = persona

colina :: Int -> Ejercicio
colina inclinacion minutos persona = quemarCalorias persona (2*minutos*inclinacion)

montaña :: Int -> Ejercicio
montaña inclinacion minutos persona = colina inclinacion (minutos `div` 2) . colina (inclinacion + 3) (minutos `div` 2) $ modificarTonificacion 1 persona

modificarTonificacion :: Int -> Persona -> Persona
modificarTonificacion unidades persona = persona {tonificacion = tonificacion persona + unidades}

{- > caminata 40 pancho
(40, 118.6 , 1) ­­­ quema 200 calorías (1*5*40)
> entrenamientoEnCinta 40 pancho
(40, 117.3 , 1)­­­ quema 400 calorías (1* (6+14/2) * 40)
> pesas 50 15 pancho
(40, 120, 6)­­­ tonifica 5 (50 / 10)
> colina 5 40 pancho
(40, 117.3 , 1)­­­ quema 400 calorías (2*40*5)
> montaña 5 40 pancho
(40, 116.5 , 2)­­­ quema 520 calorias (2*20*5 por la 1ra, 2*20*8 por la 2da) y se tonifica 1 -}

{- Rutina de ejercicios:
a. Dada una rutina (tupla con un nombre, duración total y lista de ejercicios específicos) y
una persona, obtener a la persona luego de realizar la rutina. La cantidad de minutos
dedicada a cada ejercicio es la misma.
Mostrar un ejemplo de uso usando todos los ejercicios del punto anterior.
Resolver de dos formas:
● Con recursividad
● Con fold
b. Dada una rutina y una persona, obtener el resumen de rutina que es una tupla con el
nombre de la misma, los kilos perdidos y la tonificación ganada por la persona al
realizarla. -}

type Rutina = (String, Int, [Ejercicio])

realizarRutina :: Rutina -> Persona -> Persona
realizarRutina (nombre, duracion, ejercicios) persona = foldl (flip ($)) persona (replicate (length ejercicios) (duracion `div` length ejercicios))

-- > realizarRutina ("pesas", 60, [caminata 5, pesas 50, colina 5]) pancho
-- (40, 120, 6)

resumenDeRutina :: Rutina -> Persona -> (String, Float, Int)
resumenDeRutina (nombre, duracion, ejercicios) persona = (nombre, peso persona - peso persona', tonificacion persona' - tonificacion persona)
    where persona' = realizarRutina (nombre, duracion, ejercicios) persona

{- 5. Dada una lista de rutinas, obtener un resumen de todas las que (individualmente) pueden llevar a una persona dada a estar saludable. Resolver usando composición, aplicación parcial y orden superior. -}

rutinasSaludables :: [Rutina] -> Persona -> [(String, Float, Int)]
rutinasSaludables rutinas persona = map (flip resumenDeRutina persona) (filter (flip esSaludable persona) rutinas)

esSaludable :: Rutina -> Persona -> Bool
esSaludable rutina persona = peso (realizarRutina rutina persona) < 80 && tonificacion (realizarRutina rutina persona) > 5

