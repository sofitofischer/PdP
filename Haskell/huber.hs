{-Una agencia de remises contrata los más eficientes choferes de los que conoce:
el nombre
el kilometraje de su auto
los viajes que tomó
qué condición impone para tomar un viaje

Cada viaje se hace en una fecha particular, lo toma un cliente (queremos saber su nombre y dónde vive) y tiene un costo.

En cuanto a la condición para tomar un viaje
algunos choferes toman cualquier viaje
otros solo toman los viajes que salgan más de $ 200
otros toman aquellos en los que el nombre del cliente tenga más de n letras
y por último algunos requieren que el cliente no viva en una zona determinada -}

-- (2 puntos) Modelar los TAD cliente, chofer y viaje. 

type Condicion = Viaje -> Bool

data Chofer = UnChofer{
    nombreChofer :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condicion :: Condicion
} --deriving (Show)

data Viaje = UnViaje{
    fecha :: (Int, Int, Int),
    cliente :: Cliente,
    costo :: Int
} deriving (Show)

data Cliente = UnCliente{
    nombreCliente :: String,
    lugar :: String
} deriving (Show)

-- (2 puntos) Implementar con las abstracciones que crea conveniente las condiciones que cada chofer tiene para tomar un viaje. Debe utilizar en este punto composición y aplicación parcial.

cualquierViaje :: Condicion
cualquierViaje _ = True

masDe200 :: Condicion
masDe200 = (>200) . costo

masDeNLetras :: Int -> Condicion
masDeNLetras n = (>n) . length . nombreCliente . cliente

noViveEn :: String -> Condicion
noViveEn donde = (/=donde) . lugar . cliente

-- (1 punto) Definir las siguientes expresiones: 
{- el cliente “Lucas” que vive en Victoria
el chofer “Daniel”, su auto tiene 23.500 kms., hizo un viaje con el cliente Lucas el 20/04/2017 cuyo costo fue $ 150, y toma los viajes donde el cliente no viva en “Olivos”.
la chofer “Alejandra”, su auto tiene 180.000 kms, no hizo viajes y toma cualquier viaje. -}

lucas :: Cliente
lucas = UnCliente "Lucas" "Victoria"

daniel, alejandra :: Chofer
daniel = UnChofer "Daniel" 23500 [UnViaje (20, 4, 2017) lucas 150] (noViveEn "Olivos")
alejandra = UnChofer "Alejandra" 180000 [] cualquierViaje

-- (1 punto) Saber si un chofer puede tomar un viaje.

tomaViaje :: Viaje -> Chofer -> Bool
tomaViaje viaje chofer = condicion chofer viaje

-- (2 puntos) Saber la liquidación de un chofer, que consiste en sumar los costos de cada uno de los viajes. Por ejemplo, Alejandra tiene $ 0 y Daniel tiene $ 150.

liquidacion :: Chofer -> Int
liquidacion = sum . map costo . viajes

{- (4 puntos) Realizar un viaje: dado un viaje y una lista de choferes, se pide que
filtre los choferes que toman ese viaje. Si ningún chofer está interesado, no se preocupen: el viaje no se puede realizar.
considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.
efectuar el viaje: esto debe incorporar el viaje a la lista de viajes del chofer. ¿Cómo logra representar este cambio de estado? -}

realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje = hacerViaje viaje . choferConMenosViajes . filter (tomaViaje viaje)

hacerViaje :: Viaje -> Chofer -> Chofer
hacerViaje viaje chofer = chofer {viajes = viaje : viajes chofer}

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes = foldl1 menorViajes

menorViajes :: Chofer -> Chofer -> Chofer
menorViajes chofer1 chofer2
    | length (viajes chofer1) < length (viajes chofer2) = chofer1
    | otherwise = chofer2

-- (1 punto) Al infinito y más allá
{- Modelar al chofer “Nito Infy”, su auto tiene 70.000 kms., que el 11/03/2017 hizo infinitos viajes de $ 50 con Lucas y toma cualquier viaje donde el cliente tenga al menos 3 letras. Puede ayudarse con esta función: -}

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

nitoInfy :: Chofer
nitoInfy = UnChofer "Nito Infy" 70000 (repetirViaje (UnViaje (11, 3, 2017) lucas 50)) (masDeNLetras 3)

-- ¿Puede calcular la liquidación de Nito? Justifique. ¿Y saber si Nito puede tomar un viaje de Lucas de $ 500 el 2/5/2017? Justifique. 

-- rta: No se puede calcular la liquidacion de nito porque no termina nunca
-- rta: Nito puede tomar el viaje de Lucas porque no involucra a la lista de viajes

-- (1 punto) Inferir el tipo de la función gōngnéng

gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
gongNeng arg1 arg2 arg3 = 
     max arg1 . head . filter arg2 . map arg3 


