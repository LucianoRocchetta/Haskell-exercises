module Thanos where
import Text.Show.Functions()

type Habilidad = String
type Planeta = String
type Energia = Int
type Gema = Personaje -> Personaje
type Oponente = Personaje
type Universo = [Personaje]

data Personaje = Personaje {
    nombre :: String,
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    planeta :: String
} deriving (Show, Eq)

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show)

thor :: Personaje
thor = Personaje {
    nombre = "Thor",
    edad = 40,
    energia = 200,
    habilidades = ["usar Mjolnir"],
    planeta = "Asgard"
}

ironman :: Personaje
ironman = Personaje {
    nombre = "IronMan",
    edad = 52,
    energia = 700,
    habilidades = ["tirar cohete"],
    planeta = "Tierra"
}

restarPuntosEnergia :: Energia -> Personaje -> Energia
restarPuntosEnergia energi personaje = energia personaje - energi


chasquidoAUniverso :: Universo -> Universo
chasquidoAUniverso universo = take ((length(universo)) `div` 2) (universo)

tieneMasDeDosHabilidades :: Personaje -> Bool
tieneMasDeDosHabilidades personaje = length (habilidades personaje) > 2

energiaUniverso :: Universo -> Int
energiaUniverso universo = sum (map energia (filter (tieneMasDeDosHabilidades) (universo)))

{--
    ---------
    Gemas
    ---------
--}

aplicarGema :: Gema -> Personaje -> Personaje
aplicarGema gema personaje = gema personaje

gemaDeMente :: Int -> Gema
gemaDeMente valor personaje = personaje{energia = energia personaje - valor}


{-- Hacer abstraccion de funcion lambda --}
gemaDeAlma :: Habilidad -> Gema
gemaDeAlma habilidad personaje = personaje{
    habilidades = filter (\n -> not (n == habilidad)) (habilidades personaje),
    energia = restarPuntosEnergia 10 personaje
}

gemaDeEspacio :: Planeta -> Gema
gemaDeEspacio planet personaje = personaje{
    energia = restarPuntosEnergia 20 personaje,
    planeta = planet
}

gemaDeTiempo :: Oponente -> Gema
gemaDeTiempo personaje oponente = personaje{
    edad = max ((edad oponente) `div` 2) 18,
    energia = restarPuntosEnergia 50 personaje
}

{-- VER OTRA ABSTRACCION --}
gemaLoca :: Gema -> Gema
gemaLoca gema personaje = aplicarGema gema (aplicarGema gema personaje) 

{--
    ---------
    Ejemplo de gemas
    ---------
--}

guantelet :: Guantelete 
guantelet = Guantelete {
    material = "goma",
    gemas = [gemaDeTiempo thor, gemaDeAlma "usar Mjolnir", gemaLoca (gemaDeAlma "programación en Haskell")]
}

aplicarGemas :: [Gema] -> Personaje -> Personaje
aplicarGemas gemas personaje = foldr ($) personaje gemas

{--
ordenarGemasPorEnergia :: [Energia] -> [Energia]
ordenarGemasPorEnergia energias = sort energias

aplicarGemasAPersonaje :: [Gema] -> Personaje -> [Energia]
aplicarGemasAPersonaje (gema:colaGemas) personaje = [energia (gema personaje)] ++ [aplicarGemasAPersonaje colaGemas personaje]

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa (Guantelete material (gema:colaGemas)) persona = head (ordenarGemasPorEnergia aplicarGemasAPersonaje)

--}

elegirGema :: [Gema] -> Personaje -> Gema
elegirGema (gema:gemas) personaje | (energia (gema personaje)) > (energia ((head gemas) personaje)) = elegirGema (gema:tail gemas) personaje
                                  | otherwise = elegirGema gemas personaje

gemaMasPoderosa2 :: Guantelete -> Personaje -> Gema
gemaMasPoderosa2 guantelete persona = elegirGema (gemas guantelete) persona