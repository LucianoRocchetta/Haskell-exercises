module Samurai where
import Text.Show.Functions

type Anio = Int
type Danio = Float
type Enemigo = Personaje
type Concentracion = Int
type Esbirros = [Elemento]

data Elemento = UnElemento { 
    tipo :: String,
    ataque :: (Personaje -> Personaje),
    defensa :: (Personaje -> Personaje) 
}

data Personaje = UnPersonaje { 
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int 
}

noHaceNada :: Personaje -> Personaje
noHaceNada personaje = personaje

modificarSalud :: Float -> Personaje -> Personaje
modificarSalud valor personaje = personaje{salud = max (salud personaje + valor) 0}

mandarAnio :: Anio -> Personaje -> Personaje
mandarAnio anio personaje = personaje{anioPresente = anio}

meditar :: Personaje -> Personaje
meditar personaje = modificarSalud (salud personaje / 2) personaje

causarDanio :: Danio -> Personaje -> Personaje
causarDanio danio personaje = modificarSalud danio personaje

esMalvado :: Personaje -> Bool
esMalvado personaje = any (=="Maldad") (map tipo (elementos personaje))

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - (salud . ataque elemento $ personaje)

puedeLLegarAMatarlo :: Personaje -> Enemigo -> Bool
puedeLLegarAMatarlo personaje enemigo = any (==0) . (map (danioQueProduce personaje)) $ elementos enemigo

{-- Voy a tener que usar un map, any y un filter--}
enemigosMortales :: Personaje -> [Enemigo] -> [Enemigo]
enemigosMortales personaje enemigos = filter (puedeLLegarAMatarlo personaje) $ enemigos 

{-- PUNTO 3 --}

aplicarMeditarNVeces :: Int -> Personaje -> Personaje
aplicarMeditarNVeces cantidad personaje = aplicarMeditarNVeces (cantidad - 1) personaje

concentracion :: Concentracion -> Elemento
concentracion cantidad = UnElemento {
    tipo = "Magia",
    ataque = noHaceNada,
    defensa = aplicarMeditarNVeces cantidad
}

esbirrosMalvados :: Int -> Esbirros
esbirrosMalvados cantidad = replicate cantidad 
    UnElemento{
        tipo="Maldad", 
        ataque= causarDanio 1, 
        defensa= noHaceNada
}

jack :: Personaje 
jack = UnPersonaje{
    nombre = "Jack",
    salud = 300,
    elementos = [concentracion 3, UnElemento{tipo="Magia", ataque = causarDanio 1000, defensa = noHaceNada}],
    anioPresente = 200
}

portalAlFuturo :: Int -> Elemento
portalAlFuturo anio = UnElemento{
    tipo = "Magia",
    ataque = mandarAnio (anio + 2800),
    defensa = (aku (anio + 2800) . salud)
}

aku :: Int -> Float -> Personaje
aku anio salud = UnPersonaje {
    nombre = "Aku",
    salud = salud,
    elementos = [concentracion 4] ++ esbirrosMalvados (anio * 100) ++
    {-- Portal al futuro --}
    [portalAlFuturo anio],
    anioPresente = anio
}

{-- PUNTO 4 --}

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor | salud atacante == 0 = (defensor, atacante)
                         | 