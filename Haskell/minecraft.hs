-- MINECRAFT
{- En este videojuego, existen personajes cuyo objetivo es minar materiales y construir nuevos objetos. Los personajes tienen un nombre, un puntaje y un inventario con todos los materiales que poseen. De un mismo material se pueden tener varias unidades. -}

data Personaje = UnPersonaje {
    nombre:: String,
    puntaje:: Int,
    inventario:: [Material]
} deriving Show

{- CRAFT: 
Craftear consiste en construir objetos a partir de otros objetos. Para ello se cuenta con recetas que consisten en una lista de materiales que se requieren para craftear un nuevo objeto. En ninguna receta hace falta más de una unidad de mismo material. La receta también especifica el tiempo que tarda en construirse. Todo material puede ser componente de una receta y todo objeto resultante de una receta también es un material y puede ser parte en la receta de otro.
Por ejemplo:
para hacer una fogata, se necesita madera y fósforo y se tarda 10 segundos
para hacer pollo asado, se necesita fogata y un pollo, pero se tarda 300 segundos
para hacer un sweater, se necesita lana, agujas y tintura, y demora 600 segundos

1) Hacer las funciones necesarias para que un jugador craftee un nuevo objeto
El jugador debe quedar con el nuevo objeto en su inventario
El jugador debe quedar sin los materiales usados para craftear
La cantidad de puntos del jugador se incrementa a razón de 10 puntos por segundo utilizado en el crafteo.
El objeto se craftea sólo si se cuenta con todos los materiales requeridos antes de comenzar la tarea. En caso contrario, no se altera el inventario, pero se pierden 100 puntos.
Por ejemplo, si un jugador con 1000 puntos tenía un sweater, una fogata y dos pollos y craftea un pollo asado, mantiene su sweater intacto, se queda con un sólo pollo, sin fogatas y pasa a tener un pollo asado en su inventario. Su puntaje pasa a ser 4000. -}

data Material = UnMaterial {
    nombreMaterial:: String,
    tiempo:: Int,
    receta:: Receta
} deriving (Show, Eq)

type Receta = [Material]

-- Ejemplos de materiales
madera = UnMaterial "madera" 10 []
fosforo = UnMaterial "fosforo" 10 []
pollo = UnMaterial "pollo" 10 []
fogata = UnMaterial "fogata" 10 [madera, fosforo]
polloAsado = UnMaterial "pollo asado" 300 [fogata, pollo]
lana = UnMaterial "lana" 10 []
agujas = UnMaterial "agujas" 10 []
tintura = UnMaterial "tintura" 10 []
sweater = UnMaterial "sweater" 600 [lana, agujas, tintura]

-- Ejemplos de personajes
steve = UnPersonaje "Steve" 1000 [madera, fosforo, fogata, pollo, pollo]
alex = UnPersonaje "Alex" 1000 [lana, agujas, tintura, sweater, fogata, pollo, pollo]

craftear :: Material -> Personaje -> Personaje
craftear material personaje
    | tieneMateriales material personaje = personaje {puntaje = (puntaje personaje) + (tiempo material) * 10, inventario = filter (\x -> not (elem x (receta material))) (inventario personaje) ++ [material]}

-- ejemplo de consulta: craftear polloAsado steve

tieneMateriales :: Material -> Personaje -> Bool
tieneMateriales material personaje = all (\x -> elem x (inventario personaje)) (receta material)

{- 2) Dado un personaje y una lista de recetas:
Encontrar los objetos que podría craftear un jugador y que le permitirían como mínimo duplicar su puntaje.
Hacer que un personaje craftee sucesivamente todos los objetos indicados en la lista de recetas.
Averiguar si logra quedar con más puntos en caso de craftearlos a todos sucesivamente en el orden indicado o al revés. -}

duplican :: Personaje -> Material -> Bool
duplican personaje material = (puntaje personaje) * 2 <= (puntaje (craftear material personaje))

objetosQueDuplicanPuntaje :: Personaje -> [Receta] -> [Material]
objetosQueDuplicanPuntaje personaje recetas = filter (duplican personaje) (concat recetas)

-- ejemplo de consulta: objetosQueDuplicanPuntaje alex [[polloAsado], [sweater]]

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente personaje recetas = foldl (\x y -> craftear y x) personaje (concat recetas)

-- ejemplo de consulta: craftearSucesivamente alex [[polloAsado], [sweater]]

craftearSucesivamenteAlReves :: Personaje -> [Receta] -> Personaje
craftearSucesivamenteAlReves personaje recetas = foldr (\x y -> craftear x y) personaje (concat recetas)

-- ejemplo de consulta: craftearSucesivamenteAlReves alex [[polloAsado], [sweater]]

quedaConMasPuntos :: Personaje -> [Receta] -> Bool
quedaConMasPuntos personaje recetas = (puntaje (craftearSucesivamente personaje recetas)) > (puntaje (craftearSucesivamenteAlReves personaje recetas))

-- ejemplo de consulta: quedaConMasPuntos alex [[polloAsado], [sweater]]

{- MINE:
El mundo del videojuego se compone de biomas, donde cada bioma tiene muchos materiales. Para poder minar en un bioma particular el personaje debe tener un elemento necesario según el bioma. Por ejemplo, en un bioma ártico, donde hay hielo, iglues y lobos, se debe tener un sweater.

Cuando un personaje va a minar a un bioma, si cuenta con el elemento necesario, agrega a su inventario uno de los materiales del bioma y gana 50 puntos. La forma de elegir cuál es el material del bioma a conseguir, depende de la herramienta que use al minar. Por ejemplo, el hacha hace que se mine el último de los materiales del bioma, mientras que la espada actúa sobre el primero de ellos. Existe tambien el pico, que por ser más preciso permite apuntar a una determinada posición de los materiales. Por ejemplo, si un personaje con un sweater en su inventario mina el artico con un pico de precisión 1, agrega un iglú a su inventario. En caso de no poder minar por no tener lo necesario el personaje se va con las manos vacías y sigue como antes.

1) Hacer una función minar, que dada una herramienta, un personaje y un bioma, permita obtener cómo queda el personaje. -}

data Bioma = UnBioma {
    nombreBioma:: String,
    materiales:: [Material],
    elementoNecesario:: Material
} deriving (Show, Eq)

-- Ejemplos de biomas
artico = UnBioma "artico" [hielo, iglu, lobo] sweater
desierto = UnBioma "desierto" [arena, cactus, oasis] agua

minar :: Herramienta -> Personaje -> Bioma -> Personaje
minar herramienta personaje bioma
    | tieneElementoNecesario personaje bioma = personaje {puntaje = (puntaje personaje) + 50, inventario = (inventario personaje) ++ [materialAObtener herramienta bioma]}
    | otherwise = personaje

tieneElementoNecesario :: Personaje -> Bioma -> Bool
tieneElementoNecesario personaje bioma = elem (elementoNecesario bioma) (inventario personaje)

materialAObtener :: Herramienta -> Bioma -> Material
materialAObtener herramienta bioma
    | nombreHerramienta herramienta == "hacha" = last (materiales bioma)
    | nombreHerramienta herramienta == "espada" = head (materiales bioma)
    | otherwise = (materiales bioma) !! (precision herramienta)

-- ejemplo de consulta: minar hacha steve artico

{- 2) Definir las herramientas mencionadas y agregar dos nuevas. Mostrar ejemplos de uso. Hacerlo de manera que agregar en el futuro otras herramientas no implique modificar la función minar.
Utilizando la función composición, usar una que permita obtener un material del medio del conjunto de materiales.
Utilizando una expresión lambda, inventar una nueva herramienta, diferente a las anteriores. -}

data Herramienta = UnaHerramienta {
    nombreHerramienta:: String,
    precision:: Int
} deriving (Show, Eq)

-- Ejemplos de herramientas
hacha = UnaHerramienta "hacha" 0
espada = UnaHerramienta "espada" 0
pico = UnaHerramienta "pico" 1
picoDeMadera = UnaHerramienta "picoDeMadera" 1
picoDePiedra = UnaHerramienta "picoDePiedra" 2

-- Mas ejemplos de materiales
hielo = UnMaterial "hielo" 10 [] 
iglu = UnMaterial "iglu" 20 []
lobo = UnMaterial "lobo" 30 []
arena = UnMaterial "arena" 10 [] 
cactus = UnMaterial "cactus" 20 [] 
oasis = UnMaterial "oasis" 30 []
agua = UnMaterial "agua" 40 []

-- Ejemplo de consulta: minar picoDeMadera steve desierto
-- Ejemplo 2 de consulta: minar picoDeMadera alex artico
-- Ejemplo 3 de consulta: minar picoDePiedra steve artico
-- Ejemplo 4 de consulta: minar picoDePiedra alex desierto
-- Ejemplo 5 de consulta: minar hacha steve artico
-- Ejemplo 6 de consulta: minar espada steve artico
-- Ejemplo 7 de consulta: minar pico steve artico

obtenerMaterialDelMedio :: [Material] -> Material
obtenerMaterialDelMedio materiales = (materiales) !! (div (length (materiales)) 2)

--herramientaLambda :: String -> Herramienta
--herramientaLambda nombre = UnaHerramienta nombre (\x -> length x) (nombre)

{- 3) ¿Qué pasa al intentar minar en un bioma con infinitos materiales? Mostrar ejemplos donde con diferentes herramientas o personajes sucedan diferentes cosas. Justificar. -}

-- No se puede minar en un bioma con infinitos materiales, ya que no se puede obtener el material a minar, ya que no se puede obtener el último o el primero de una lista infinita.

-- Ejemplo de consulta: minar hacha steve biomaInfinito

biomaInfinito = UnBioma "biomaInfinito" [hielo, iglu, lobo] sweater