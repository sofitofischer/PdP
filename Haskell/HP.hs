-- ej 1.A)
data Postre = UnPostre {
    nomPostre :: String,
    sabor :: [String],
    peso :: Float,
    temperatura :: Float
} deriving (Show, Eq)

data Hechizo = UnHechizo {
    nomHechizo :: String,
    efecto :: [Postre -> Postre]
} --deriving (Show)

data Mago = UnMago {
    nomMago :: String,
    hechizosAprendidos :: [Hechizo],
    horrorcruxes :: Int
} --deriving (Show)

-- ej 1.B)
incendio, immobulus, wingardiumLeviosa, diffindo, riddikulus, avadaKedavra :: Hechizo
incendio = UnHechizo {
    nomHechizo = "Incendio",
    efecto = [calentarPostre 1, perderPeso 0.95] 
}
immobulus = UnHechizo {
    nomHechizo = "Immobulus",
    efecto = [congelarPostre]
}
wingardiumLeviosa = UnHechizo {
    nomHechizo = "Wingardium Leviosa",
    efecto = [agregarSabor "Concentrado", perderPeso 0.9]
}
diffindo = UnHechizo {
    nomHechizo = "Diffindo",
    efecto = [perderPeso 0.5]
}
riddikulus = UnHechizo {
    nomHechizo = "Riddikulus",
    efecto = [agregarSaborInvertido "sab"]
}
avadaKedavra = UnHechizo {
    nomHechizo = "Avada Kedavra",
    efecto = [congelarPostre, perderSabores]
}

calentarPostre :: Float -> Postre -> Postre
calentarPostre n postre = postre {temperatura = temperatura postre + n}

perderPeso :: Float -> Postre -> Postre
perderPeso n postre = postre {peso = peso postre*n}

congelarPostre :: Postre -> Postre
congelarPostre postre = postre {temperatura = 0}

agregarSabor :: String -> Postre -> Postre
agregarSabor sab postre = postre {sabor = sabor postre ++ [sab]}

agregarSaborInvertido :: String -> Postre -> Postre
agregarSaborInvertido sab postre = postre {sabor = sabor postre ++ [reverse sab]}

perderSabores :: Postre -> Postre
perderSabores postre = postre {sabor = []}

-- ej 1.C)
bizcochoBorracho, tartaDeMelaza, helado, flan :: Postre

bizcochoBorracho = UnPostre {
    nomPostre = "Bizcocho",
    sabor = ["Fruta", "Crema"],
    peso = 100,
    temperatura = 25
}
tartaDeMelaza = UnPostre {
    nomPostre = "Tarta",
    sabor = ["Miel", "Crema", "Mantequilla"],
    peso = 50,
    temperatura = 0
}
helado = UnPostre {
    nomPostre = "Helado",
    sabor = ["Vainilla", "Chocolate"],
    peso = 200,
    temperatura = -10
}
flan = UnPostre {
    nomPostre = "Flan",
    sabor = ["Dulce de leche", "Vainilla"],
    peso = 250,
    temperatura = 10
}

-- Dado un conjunto de postres en la mesa, saber si hacerles un determinado hechizo los dejará listos (un postre está listo cuando pesa algo más que cero, tiene algún sabor y además no está congelado).

estanListos :: Hechizo -> [Postre] -> Bool
estanListos hechizo = all (listo . aplicarHechizo hechizo)

listo :: Postre -> Bool
listo postre = peso postre > 0 && (not.null.sabor) postre && temperatura postre > 0

aplicarHechizo :: Hechizo -> Postre -> Postre
aplicarHechizo hechizo postre = foldr ($) postre (efecto hechizo)

-- ej 1.D)
-- Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos.

pesoPromedio :: [Postre] -> Float
pesoPromedio postres = (sum.map peso.filter listo) postres / fromIntegral (length (filter listo postres))

{-
Magos
De un mago se conocen sus hechizos aprendidos y la cantidad de horrorcruxes que tiene.
Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un hechizo sobre un postre (se espera obtener el mago). Cuando un mago practica con un hechizo, lo agrega a sus hechizos aprendidos. 
Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada kedavra” al postre, entonces suma un horrorcrux.
Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al postre con más cantidad de sabores luego de usarlo.
-}

-- ej 2.A)
merlin, hermione, voldemort :: Mago
merlin = UnMago {
    nomMago = "Merlin",
    hechizosAprendidos = [incendio, immobulus, wingardiumLeviosa, diffindo],
    horrorcruxes = 0
}
hermione = UnMago {
    nomMago = "Hermione",
    hechizosAprendidos = [incendio, immobulus, wingardiumLeviosa, diffindo, riddikulus],
    horrorcruxes = 0
}
voldemort = UnMago {
    nomMago = "Voldemort",
    hechizosAprendidos = [avadaKedavra],
    horrorcruxes = 0
}

practicarHechizo :: Mago -> Hechizo -> Postre -> Mago 
practicarHechizo mago hechizo postre = mago {hechizosAprendidos = hechizosAprendidos mago ++ [hechizo], horrorcruxes = horrorcruxes mago + parecidoAAvadaKedavra hechizo postre}

parecidoAAvadaKedavra :: Hechizo -> Postre -> Int
parecidoAAvadaKedavra hechizo postre 
    | postre == aplicarHechizo avadaKedavra postre = 1
    | otherwise = 0

-- ej 2.B)
mejorHechizo :: Mago -> Postre -> Hechizo
mejorHechizo mago postre = foldr1 (compararHechizos postre) (hechizosAprendidos mago)

compararHechizos :: Postre -> Hechizo -> Hechizo -> Hechizo
compararHechizos postre hechizo1 hechizo2
    | length (sabor (aplicarHechizo hechizo1 postre)) > length (sabor (aplicarHechizo hechizo2 postre)) = hechizo1
    | otherwise = hechizo2

{-
ej 3.A) Construir una lista infinita de postres, y construir un mago con infinitos hechizos.
ej 3.B) Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar conceptualmente.
ej 3.C) Suponiendo que un mago tiene infinitos hechizos ¿Existe algún caso en el que se puede encontrar al mejor hechizo? Justificar conceptualmente.
-}

-- ej 3.A)
postresInfinitos :: [Postre]
postresInfinitos = bizcochoBorracho : postresInfinitos

{-magoHechizosInfinitos :: Mago
magoHechizosInfinitos = UnMago {
    nomMago = "Mago",
    hechizosAprendidos = incendio : magoHechizosInfinitos,
    horrorcruxes = 0
}-}

-- ej 3.B) No, porque Haskell es un lenguaje lazy, por lo que no evalúa la lista infinita de postres, por lo que no puede saber si algún hechizo los deja listos.

-- ej 3.C) No, porque Haskell es un lenguaje lazy, por lo que no evalúa la lista infinita de hechizos, por lo que no puede saber cuál es el mejor hechizo.

