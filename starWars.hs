module StarWars where
import Text.Show.Functions()

type Poder = Nave -> Nave
type Ataque = Int
type Durabilidad = Int
type Escudo = Int
type Flota = [Nave]
type NaveAtacada = Nave

type Estrategia = Flota -> Nave -> Flota

data Nave = Nave {
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: Poder
} deriving (Show)

tieFighter :: Nave
tieFighter = Nave {
    nombre = "Tie Fighter",
    durabilidad = 200,
    escudo = 100,
    ataque = 50,
    poder = movimientoTurbo
}

xWing :: Nave
xWing = Nave {
    nombre = "X Wing",
    durabilidad = 300,
    escudo = 150,
    ataque = 100,
    poder = reparacionDeEmergencia
}

naveDarthVader :: Nave
naveDarthVader = Nave {
    nombre = "Nave de Darth Vader",
    durabilidad = 500,
    escudo = 300,
    ataque = 200,
    poder = movimientoSuperTurbo
}

millenniumFalcon :: Nave
millenniumFalcon = Nave {
    nombre = "Millennium Falcon",
    durabilidad = 1000,
    escudo = 500,
    ataque = 60,
    poder = modificarEscudo 100 . reparacionDeEmergencia
}

{-- 
Abstraccion mayor 
modificarNave :: (Int -> Nave) -> Int -> Nave -> Nave
modificarNave campo valor nave =  
--}

modificarDurabilidad :: Durabilidad -> Nave -> Nave
modificarDurabilidad valor nave = nave{durabilidad = durabilidad nave + valor}

modificarAtaque :: Ataque -> Nave -> Nave
modificarAtaque valor nave = nave{ataque = ataque nave + valor}

modificarEscudo :: Escudo -> Nave -> Nave
modificarEscudo valor nave = nave{escudo = ataque nave + valor}

{-- PODERES --}

movimientoTurbo :: Poder
movimientoTurbo nave = modificarAtaque 25 nave

movimientoSuperTurbo :: Poder
movimientoSuperTurbo nave = modificarDurabilidad (-45) .movimientoTurbo.movimientoTurbo.movimientoTurbo $ nave

reparacionDeEmergencia :: Poder
reparacionDeEmergencia nave = modificarDurabilidad 50 . modificarAtaque (-30) $ nave

{-- El map devuelve una lista con cada una de las durabilidades de las naves--}
calcularDurabilidadTotal :: Flota -> Int
calcularDurabilidadTotal naves = sum (map durabilidad naves)

calcularDañoRecibido :: Nave -> NaveAtacada -> Int
calcularDañoRecibido nave1 nave2 | escudo nave2 > ataque nave1 = 0
                                 | otherwise  = ataque nave1 - escudo nave2

ataqueEntreNaves :: Nave -> NaveAtacada -> NaveAtacada
ataqueEntreNaves nave1 nave2 = nave2{durabilidad = max (durabilidad nave2 - calcularDañoRecibido (poder nave1 $ nave1) (poder nave2 $ nave2)) 0}

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate nave = durabilidad nave == 0

{-- ESTRATEGIAS --}
atacarNavesDebiles :: Estrategia
atacarNavesDebiles naves nave = map (ataqueEntreNaves nave) (filter (<200) (map escudo naves))

atacarNavesConCiertaPeligrosidad :: Ataque -> Estrategia
atacarNavesConCiertaPeligrosidad valor naves nave = map (ataqueEntreNaves nave) (filter (>valor) (map ataque naves))

atacarNavesFueraDeCombate :: Estrategia
atacarNavesFueraDeCombate naves nave = filter (estaFueraDeCombate) (map (ataqueEntreNaves nave) naves)

compararDurabilidades :: Nave -> Flota -> Estrategia -> Estrategia -> Bool
compararDurabilidades nave naves estrategia1 estrategia2 = calcularDurabilidadTotal (estrategia1 naves nave) > calcularDurabilidadTotal (estrategia2 naves nave)

realizarMision :: Nave -> Flota -> Estrategia -> Estrategia -> Flota
realizarMision nave naves estrategia1 estrategia2 | (compararDurabilidades nave naves estrategia1 estrategia2) = estrategia1 naves nave
                                                  | otherwise = estrategia2 naves nave