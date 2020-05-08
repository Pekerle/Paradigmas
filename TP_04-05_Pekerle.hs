module Main where
import Text.Show.Functions

main :: IO ()
main = return ()

type Nombre = String
type CantidadDeDinero = Float
type Tactica = String
type NombrePropiedad = String
type PrecioPropiedad = Float
type Accion = Jugador -> Jugador

type Acciones = [Accion]
type Propiedades = [Propiedad]

data Propiedad = UnaPropiedad {
    nombrePropiedad :: NombrePropiedad,
    precioPropiedad :: PrecioPropiedad
} deriving (Show, Eq)

data Jugador = UnJugador {
    nombre :: Nombre,
    cantidadDeDinero :: CantidadDeDinero,
    tactica :: Tactica,
    propiedades :: Propiedades,
    acciones :: Acciones
} deriving Show

carolina :: Jugador
carolina = UnJugador "Carolina" 500 "Accionista" [] [pasarPorElBanco]

manuel :: Jugador
manuel = UnJugador "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse]

modificarNombre :: (Nombre -> Nombre) -> Jugador -> Jugador
modificarNombre unaFuncion unJugador = unJugador {nombre = unaFuncion(nombre unJugador)}

modificarCantidadDeDinero :: (CantidadDeDinero -> CantidadDeDinero) -> Jugador -> Jugador
modificarCantidadDeDinero unaFuncion unJugador = unJugador {cantidadDeDinero = unaFuncion(cantidadDeDinero unJugador)}

cambiarTactica :: Tactica -> Jugador -> Jugador
cambiarTactica unaTactica unJugador = unJugador {tactica = unaTactica}

agregarPropiedad :: Propiedad -> Jugador -> Jugador
agregarPropiedad unaPropiedad unJugador = unJugador {propiedades = propiedades unJugador ++ [unaPropiedad]}

agregarAccion :: Accion -> Jugador -> Jugador
agregarAccion unaAccion unJugador = unJugador {acciones = acciones unJugador ++ [unaAccion]}

pasarPorElBanco :: Jugador -> Jugador
pasarPorElBanco unJugador = (modificarCantidadDeDinero (+ 500) . cambiarTactica "Comprador compulsivo" . agregarAccion pasarPorElBanco) unJugador

enojarse :: Jugador -> Jugador
enojarse unJugador = (modificarCantidadDeDinero (+ 50) . agregarAccion enojarse . agregarAccion gritar) unJugador

gritar :: Jugador -> Jugador
gritar unJugador = (modificarNombre ("AHHHH" ++) . agregarAccion gritar) unJugador

esTacticaBuscada :: Tactica -> Bool
esTacticaBuscada unaTactica = elem unaTactica ["Accionista", "Oferente singular"]

subastar :: Propiedad -> Jugador -> Jugador
subastar unaPropiedad unJugador
    | (esTacticaBuscada.tactica) unJugador = (modificarCantidadDeDinero (subtract (precioPropiedad unaPropiedad)) . agregarAccion (subastar unaPropiedad) ) unJugador
    | otherwise = unJugador

cobroUnitario :: Propiedad -> Float
cobroUnitario unaPropiedad
    | precioPropiedad unaPropiedad < 150 = 10
    | otherwise                          = 20

totalDeAlquileres :: Propiedades -> Float
totalDeAlquileres unasPropiedades = (sum.map cobroUnitario) unasPropiedades

cobrarAlquileres :: Jugador -> Jugador
cobrarAlquileres unJugador 
    | propiedades unJugador == [] = unJugador
    | otherwise                   = (modificarCantidadDeDinero (+ totalDeAlquileres (propiedades unJugador)) . agregarAccion cobrarAlquileres) unJugador

pagarAAccionistas :: Jugador -> Jugador
pagarAAccionistas unJugador
    | tactica unJugador == "Accionista" = (modificarCantidadDeDinero (+200)         . agregarAccion pagarAAccionistas) unJugador
    | otherwise                         = (modificarCantidadDeDinero (subtract 100) . agregarAccion pagarAAccionistas) unJugador