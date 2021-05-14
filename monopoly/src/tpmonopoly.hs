{-- Tp Monopoly
Marcos Decima De Pietro
Pdp Lunes Mañana--}
import Text.Show.Functions

data Persona= Persona {
    nombre  :: String,
    dinero :: Int,
    tactica :: String,
    propiedades :: [Propiedades],
    acciones :: [String]
} deriving (Show)

type NombrePropiedad = String
type PrecioPropiedad = Int
type Propiedades =(NombrePropiedad,PrecioPropiedad)

{--data Propiedades = UnaPropiedad {
    nombrePropiedad :: String,
    precioPropiedad :: Int
} deriving (Show)--}

accionARealizar :: String -> Persona -> Persona
accionARealizar accion unaPersona = unaPersona {acciones = acciones unaPersona ++ [accion] } 

carolina :: Persona 
carolina = Persona "Carolina" 500 "Accionista" [] ["pasarPorElBanco","pagarAAccionistas"]

manuel :: Persona
manuel = Persona "Manuel" 500 "Oferente singular" [] ["pasarPorElBanco","enojarse"] 