module Test where

{--
    Some types
--}
type Idioma = String
type Minutos = Int
type Intensidad = Int
type IntensidadMarea = String
type Excursion = Turista -> Turista
type Porcentaje = Int
type Indice = Turista -> Int
type Tour = [Excursion]

data Turista = Turista {
    nivelCansancio :: Int,
    nivelStress :: Int,
    viajaSolo :: Bool,
    idiomas :: [Idioma]
} deriving (Show, Eq)

ana :: Turista 
ana = Turista {
    nivelCansancio = 0,
    nivelStress = 21,
    viajaSolo = False,
    idiomas = ["Espanol"]
}

beto :: Turista 
beto = Turista {
    nivelCansancio = 15,
    nivelStress = 15,
    viajaSolo = True,
    idiomas = ["Aleman"]
}

cathi :: Turista 
cathi = Turista {
    nivelCansancio = 15,
    nivelStress = 15,
    viajaSolo = True,
    idiomas = ["Aleman"]
}


{--
    -------------
    IR A LA PLAYA
    -------------
--}
 
irALaPlaya :: Excursion
irALaPlaya turista | viajaSolo turista = turista{nivelCansancio = nivelCansancio turista - 5}
                   | otherwise = turista{nivelStress = nivelStress turista - 1}

{--
    -------------
    Apreciar algun elemento del paisaje
    -------------
--}
apreciarElementoPaisaje :: String -> Excursion
apreciarElementoPaisaje paisaje turista = turista{nivelStress = nivelStress turista - length(paisaje)}

{--
    -------------
    Salir a hablar un idioma
    -------------
--}

hablarNuevoIdioma :: Idioma -> Excursion
hablarNuevoIdioma idioma turista | (elem idioma (idiomas turista)) = turista{viajaSolo = False}
                                 | otherwise = turista{viajaSolo = False, idiomas = idiomas turista ++ [idioma]}

{--
    -------------
    Caminar
    -------------
--}

calcularIntensidad :: Minutos -> Intensidad
calcularIntensidad minutos = minutos `div` 4

caminar ::Minutos -> Excursion
caminar minutos turista = turista{
    nivelCansancio = nivelCansancio turista + calcularIntensidad(minutos),
    nivelStress = nivelStress turista - calcularIntensidad(minutos)
} 

{--
    -------------
    Paseo en Barco
    -------------
--}

mareaFuerte :: Excursion
mareaFuerte turista = turista{
    nivelCansancio = nivelCansancio turista + 10,
    nivelStress = nivelStress turista + 6
}

mareaTranquila :: Excursion
mareaTranquila turista = (hablarNuevoIdioma "Aleman").(apreciarElementoPaisaje "mar").(caminar 10) $ turista

paseoEnBarco :: IntensidadMarea -> Excursion
paseoEnBarco intensidadMarea turista | intensidadMarea == "Fuerte" = mareaFuerte(turista)
                                     | intensidadMarea == "Tranquila" = mareaTranquila(turista)
                                     | otherwise = turista
-- Turista -> Turista -> Turista

-- > cathi paseoEnBarco (parametros) -> cathi modifcado

calcularPorcentaje :: Porcentaje -> Excursion
calcularPorcentaje porcentaje turista = turista{nivelStress = nivelStress turista - (nivelStress turista * porcentaje `div` 100)}

aplicarExcursion :: Turista -> Excursion -> Turista
aplicarExcursion turista excursion = calcularPorcentaje 10 (excursion turista)

{--
    -------------
    DeltaExcursionSegun
    -------------
--}

deltaExcursionSegun :: Indice -> Excursion -> Turista -> Int
deltaExcursionSegun indice excursion turista = indice (aplicarExcursion turista excursion) - indice turista 

{--
    -------------
    Excursiones educativas
    -------------
--}

calcularLongitudLista :: Turista -> Int
calcularLongitudLista turista = length(idiomas turista)

esExcursionEducativa :: Turista -> Excursion -> Bool
esExcursionEducativa turista excursion = deltaExcursionSegun (calcularLongitudLista) excursion turista > 0


{--
    -------------
    Excursiones desestresantes
    -------------
--}

esExcursionDesestresante :: Turista  -> Excursion -> Bool
esExcursionDesestresante turista excursion = abs (deltaExcursionSegun (nivelStress) excursion turista) >= 3

{--
    -------------
    Tours
    -------------
--}

tourCompleto :: Tour
tourCompleto = [(caminar 20), (apreciarElementoPaisaje "cascada"), (caminar 40),  irALaPlaya, (hablarNuevoIdioma "melmacquiano")]


tourLadoB :: Excursion -> Tour
tourLadoB excursion = [(caminar 120), excursion, (paseoEnBarco "Tranquila")]

tourIslaVecina :: IntensidadMarea -> Tour
tourIslaVecina marea | marea == "Fuerte" = [paseoEnBarco(marea), (apreciarElementoPaisaje "lago")]
                             | otherwise = [paseoEnBarco(marea), irALaPlaya]

-- foldl (++) turista excursiones
hacerUnTour :: Turista -> Tour -> Turista
hacerUnTour turista excursiones = foldr ($) (turista{nivelStress = nivelStress turista + length(excursiones)}) excursiones


estaAcompanyEsDesestresante :: Turista -> Excursion -> Bool
estaAcompanyEsDesestresante turista excursion = viajaSolo (excursion turista) && esExcursionDesestresante turista excursion

esConvicente :: Tour -> Turista -> Bool
esConvicente excursiones turista = any (estaAcompanyEsDesestresante turista) excursiones


{--
    -------------
    Espiritualidad 
    -------------
--}

calcularEspiritualidad :: Turista -> Tour -> Int
calcularEspiritualidad turista tour = 20

efectividadTour :: [Turista] -> Tour -> Int
efectividadTour turistas excursiones = filter (esConvicente excursiones) turistas