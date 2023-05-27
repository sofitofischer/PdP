data Personaje = UnPersonaje {
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int 
} deriving (Show, Eq)

data Elemento = UnElemento { 
    tipo :: String,
    ataque :: Personaje -> Personaje,
    defensa :: Personaje -> Personaje 
}

instance Show Elemento where
  show :: Elemento -> String
  show = tipo

instance Eq Elemento where
  (==) :: Elemento -> Elemento -> Bool
  (==) elemento1 elemento2 = tipo elemento1 == tipo elemento2

{- Lo esperado es poder usar el efecto de ataque de un elemento sobre el rival y el de defensa sobre el personaje que lo tiene. En caso de que no se indique cuál es el efecto defensivo o el ofensivo, significa que no se altera de ninguna forma al personaje recibido.

Empecemos por algunas transformaciones básicas:
mandarAlAnio: lleva al personaje al año indicado.
meditar: le agrega la mitad del valor que tiene a la salud del personaje.
causarDanio: le baja a un personaje una cantidad de salud dada.
Hay que tener en cuenta al modificar la salud de un personaje que ésta nunca puede quedar menor a 0.
Importante: no repetir lógica. -}

mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = modificarSalud (salud personaje / 2) personaje

causarDanio :: Float -> Personaje -> Personaje
causarDanio cantidad = modificarSalud (-cantidad)

modificarSalud :: Float -> Personaje -> Personaje
modificarSalud cantidad personaje = personaje {salud = max 0 (salud personaje + cantidad)}

{- Queremos poder obtener algo de información extra sobre los personajes. Definir las siguientes funciones:
esMalvado, que retorna verdadero si alguno de los elementos que tiene el personaje en cuestión es de tipo “Maldad”.
danioQueProduce :: Personaje -> Elemento -> Float, que retorne la diferencia entre la salud inicial del personaje y la salud del personaje luego de usar el ataque del elemento sobre él.
enemigosMortales que dado un personaje y una lista de enemigos, devuelve la lista de los enemigos que pueden llegar a matarlo con un solo elemento. Esto sucede si luego de aplicar el efecto de ataque del elemento, el personaje queda con salud igual a 0. -}

esMalvado :: Personaje -> Bool
esMalvado = any ((== "Maldad") . tipo) . elementos

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje = filter (puedeMatarloConUnElemento personaje)

puedeMatarloConUnElemento :: Personaje -> Personaje -> Bool
puedeMatarloConUnElemento personaje = any (tieneAtaqueMortal personaje) . elementos

tieneAtaqueMortal :: Personaje -> Elemento -> Bool
tieneAtaqueMortal personaje elemento = (((==0).salud) . ataque elemento) personaje

-- Definir los siguientes personajes y elementos:
-- Definir concentracion de modo que se pueda obtener un elemento cuyo efecto defensivo sea aplicar meditar tantas veces como el nivel de concentración indicado y cuyo tipo sea "Magia".

concentracion :: Int -> Elemento
concentracion nivel = UnElemento "Magia" id ((!! nivel) . iterate meditar)
-- defensa = foldr1 (.) (replicate nivelDeConcentracion meditar)

-- Definir esbirrosMalvados que recibe una cantidad y retorna una lista con esa cantidad de esbirros (que son elementos de tipo “Maldad” cuyo efecto ofensivo es causar un punto de daño).

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad (UnElemento "Maldad" (causarDanio 1) id)

-- Definir jack de modo que permita obtener un personaje que tiene 300 de salud, que tiene como elementos concentración nivel 3 y una katana mágica (de tipo "Magia" cuyo efecto ofensivo es causar 1000 puntos de daño) y vive en el año 200.

jack :: Personaje
jack = UnPersonaje "jack" 300 [concentracion 3, katanaMagica] 200

katanaMagica :: Elemento
katanaMagica = UnElemento "Magia" (causarDanio 1000) id

{- Definir aku :: Int -> Float -> Personaje que recibe el año en el que vive y la cantidad de salud con la que debe ser construido. Los elementos que tiene dependerán en parte de dicho año. Los mismos incluyen:
Concentración nivel 4
Tantos esbirros malvados como 100 veces el año en el que se encuentra.
Un portal al futuro, de tipo “Magia” cuyo ataque es enviar al personaje al futuro (donde el futuro es 2800 años después del año indicado para aku), y su defensa genera un nuevo aku para el año futuro correspondiente que mantenga la salud que tenga el personaje al usar el portal. -}

aku :: Int -> Float -> Personaje
aku anio saludInicial = UnPersonaje "Aku" saludInicial (concentracion 4 : esbirrosMalvados (anio * 100) ++ [portalAlFuturo anio]) anio

portalAlFuturo :: Int -> Elemento
portalAlFuturo anio = UnElemento "Magia" (mandarAlAnio (anio + 2800)) (aku (anio + 2800) . salud)

{- Finalmente queremos saber cómo puede concluir la lucha entre Jack y Aku. Para ello hay que definir la función luchar :: Personaje -> Personaje -> (Personaje, Personaje) donde se espera que si el primer personaje (el atacante) está muerto, retorne la tupla con el defensor primero y el atacante después, en caso contrario la lucha continuará invirtiéndose los papeles (el atacante será el próximo defensor) luego de que ambos personajes se vean afectados por el uso de todos los elementos del atacante.

O sea que si luchan Jack y Aku siendo Jack el primer atacante, Jack se verá afectado por el poder defensivo de la concentración y Aku se verá afectado por el poder ofensivo de la katana mágica, y la lucha continuará con Aku (luego del ataque) como atacante y con Jack (luego de la defensa) como defensor. -}

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
    | estaMuerto atacante = (defensor, atacante)
    | otherwise = luchar proximoAtacante proximoDefensor
    where proximoAtacante = usarElementos ataque defensor (elementos atacante)
          proximoDefensor = usarElementos defensa atacante (elementos atacante)

usarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
usarElementos f personaje elementos = foldl (flip ($)) personaje (map f elementos)

estaMuerto :: Personaje -> Bool
estaMuerto = (==0) . salud

-- Inferir el tipo de la siguiente función:
f :: (Eq t1, Num t2) => (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))
