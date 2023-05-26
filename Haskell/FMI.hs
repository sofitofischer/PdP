data Pais = UnPais {
    ingresoPerCapita :: Float,
    poblacionSectorPublico :: Int,
    poblacionSectorPrivado :: Int,
    recursosNaturales :: [String],
    deuda :: Float
} deriving (Show, Eq)

namibia, venezuela, argentina, canada :: Pais
namibia = UnPais {
    ingresoPerCapita = 4140,
    poblacionSectorPublico = 400000,
    poblacionSectorPrivado = 650000,
    recursosNaturales = ["Mineria", "Ecoturismo"],
    deuda = 50
}
venezuela = UnPais {
    ingresoPerCapita = 4800,
    poblacionSectorPublico = 2000000,
    poblacionSectorPrivado = 4000000,
    recursosNaturales = ["Mineria", "Petroleo", "Ecoturismo"],
    deuda = 150
}
argentina = UnPais {
    ingresoPerCapita = 10000,
    poblacionSectorPublico = 5000000,
    poblacionSectorPrivado = 5000000,
    recursosNaturales = ["Mineria", "Petroleo", "Ecoturismo"],
    deuda = 200
}
canada = UnPais {
    ingresoPerCapita = 43200,
    poblacionSectorPublico = 2000000,
    poblacionSectorPrivado = 2000000,
    recursosNaturales = ["Ecoturismo"],
    deuda = 100
}

type Receta = [Pais -> Pais]

prestarPlataAUnPais :: Float -> Pais -> Pais
prestarPlataAUnPais n pais = pais {deuda = deuda pais + n*1.5}

reducirPuestosSectorPublico :: Int -> Pais -> Pais
reducirPuestosSectorPublico n pais
    | n > 100 = pais {poblacionSectorPublico = poblacionSectorPublico pais - n, ingresoPerCapita = ingresoPerCapita pais *0.8}
    | otherwise = pais {poblacionSectorPublico = poblacionSectorPublico pais - n, ingresoPerCapita = ingresoPerCapita pais *0.85}

explotacionDeRecursosNaturales :: String -> Pais -> Pais
explotacionDeRecursosNaturales recursoNatural pais = pais {deuda = deuda pais - 2, recursosNaturales = filter (/= recursoNatural) (recursosNaturales pais)}

blindaje :: Pais -> Pais
blindaje pais = prestarPlataAUnPais (pbi pais / 2) (reducirPuestosSectorPublico 500 pais)

pbi :: Pais -> Float
pbi pais = ingresoPerCapita pais * fromIntegral (poblacionSectorPublico pais + poblacionSectorPrivado pais)

prestar200MillonesYExplotarMineria :: Pais -> Pais
prestar200MillonesYExplotarMineria = explotacionDeRecursosNaturales "Mineria". prestarPlataAUnPais 200

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldr ($) pais receta

tienePetroleo :: [Pais] -> [Pais]
tienePetroleo = filter (elem "Petroleo".recursosNaturales)

totalDeuda :: [Pais] -> Float
totalDeuda = sum.map deuda

-- aparecieron los conceptos de Orden Superior, Composición y Aplicación Parcial en totalDeuda y tienenPetroleo, son una ventaja porque se puede reutilizar código y no repetirlo

estaOrdenadoDeMenorAMayorPBI :: Pais -> Receta -> Bool
estaOrdenadoDeMenorAMayorPBI pais recetas = and (zipWith (<=) (map(pbi . aplicarReceta recetas)(iterate (aplicarReceta recetas) pais)) (tail (map(pbi . aplicarReceta recetas)(iterate (aplicarReceta recetas) pais))))

{- Punto 5 - 2 puntos
dado un país y una lista de recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, 
en base al siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor
estaOrdenado :: Pais -> [Receta] -> Bool
estaOrdenado pais [receta] = True
estaOrdenado pais (receta1:receta2:recetas)
     = revisarPBI receta1 pais <= revisarPBI receta2 pais && estaOrdenado pais (receta2:recetas)
     where revisarPBI receta = pbi . aplicarReceta receta
-}

--estaOrdenadoDeMenorAMayorPBI pais recetas = pbi (aplicarReceta (head recetas) pais) <= pbi (aplicarReceta (head (tail recetas)) pais)

--estaOrdenadoDeMenorAMayorPBI argentina [prestar200MillonesYExplotarMineria, blindaje] 


--recursosNaturalesInfinitos :: [String]
--recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos


