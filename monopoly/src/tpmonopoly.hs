{-- Tp Monopoly
Marcos Decima De Pietro
Pdp Lunes Mañana--}
import Text.Show.Functions

data Persona= Persona {
    nombre  :: String,
    dinero :: Int,
    tactica :: String,
    propiedad :: [Propiedad],
    acciones :: [Accion]
} deriving (Show)


data Propiedad = UnaPropiedad {
    nombrePropiedad :: String,
    precioPropiedad :: Int
} deriving (Show)

type Accion = Persona -> Persona

{--type NombrePropiedad = String
type PrecioPropiedad = Int
type Propiedad =(NombrePropiedad,PrecioPropiedad)--}

accionARealizar :: Accion -> Persona -> Persona
accionARealizar accion unaPersona = accion unaPersona {acciones = acciones unaPersona ++ [accion] } 

pasarPorElBanco :: Accion
pasarPorElBanco unaPersona = sumarDinero 40 unaPersona {tactica = "Comprador compulsivo"}

sumarDinero :: Int -> Persona -> Persona
sumarDinero monto unaPersona = unaPersona {dinero = (+monto).dinero $ unaPersona}

restarDinero :: Int -> Persona -> Persona
restarDinero  monto unaPersona = unaPersona  {dinero = (dinero unaPersona) - monto}

enojarse :: Accion
enojarse unaPersona = (sumarDinero 50).accionARealizar gritar $ unaPersona

gritar :: Accion
gritar unaPersona = unaPersona { nombre = "AHHHH " ++ nombre unaPersona }


ganarSubasta :: Persona -> Bool
ganarSubasta unaPersona = (flip elem controlarTactica).tactica $ unaPersona

controlarTactica :: [String]
controlarTactica = ["Accionista", "Oferente singular"]

subastar :: Propiedad -> Accion
subastar unaPropiedad unaPersona
    | ganarSubasta unaPersona && controlarFondos unaPropiedad unaPersona = adquirirPropiedad unaPropiedad unaPersona
    | otherwise = unaPersona


adquirirPropiedad :: Propiedad -> Accion
adquirirPropiedad unaPropiedad unaPersona =  restarDinero (precioPropiedad unaPropiedad)  unaPersona  {propiedad = propiedad unaPersona ++[unaPropiedad]} 

propiedadBarata :: Propiedad -> Bool
propiedadBarata unaPropiedad = precioPropiedad unaPropiedad < 150


valorDeAlquiler :: Propiedad -> Int
valorDeAlquiler unaPropiedad
    | propiedadBarata unaPropiedad = 10
    | otherwise = 20

cobrarAlquileres :: Accion
cobrarAlquileres unaPersona = (flip sumarDinero unaPersona).sum.(map valorDeAlquiler).propiedad$ unaPersona

pagarAAccionistas :: Accion
pagarAAccionistas unaPersona 
    | tactica unaPersona == "Accionista" = sumarDinero 200 unaPersona
    | otherwise = restarDinero 100 unaPersona

controlarFondos :: Propiedad -> Persona -> Bool
controlarFondos unaPropiedad unaPersona = dinero unaPersona >= precioPropiedad unaPropiedad

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unPersona
    | controlarFondos unaPropiedad unPersona = adquirirPropiedad unaPropiedad unPersona
    | otherwise = (hacerBerrinchePor $unaPropiedad).(sumarDinero 10). gritar $ unPersona 

ultimaRonda :: Persona-> Accion
ultimaRonda unaPersona= (foldr (.) id ).acciones $ unaPersona

definirGanador :: Persona -> Persona -> Persona
definirGanador unaPersona otraPersona
    | dinero unaPersona > dinero  otraPersona  =  unaPersona 
    | otherwise =  otraPersona

juegoFinal :: Persona -> Persona -> Persona
juegoFinal unaPersona otraPersona = definirGanador (ultimaRonda unaPersona $ unaPersona) (ultimaRonda otraPersona $ otraPersona)


{--Personas de prueba--}

carolina :: Persona 
carolina = Persona "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Persona
manuel = Persona "Manuel" 500 "Oferente singular" [] [pasarPorElBanco,enojarse] 

{--Propiedades de prueba--}
mansion :: Propiedad
mansion = UnaPropiedad "Mansion" 550 

depto :: Propiedad
depto = UnaPropiedad "Departamento con vista al rio" 150 

choza :: Propiedad
choza =  UnaPropiedad "Choza humilde" 100
