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
gritar unaPersona = unaPersona { nombre = agregarPrefijo ++ nombre unaPersona }

agregarPrefijo :: String 
agregarPrefijo = "AHHHH "

ganarSubasta :: Persona -> Bool
ganarSubasta unaPersona = (flip elem controlarTactica).tactica $ unaPersona

controlarTactica :: [String]
controlarTactica = ["Accionista", "Oferente singular"]

subastar :: Propiedad -> Accion
subastar unaPropiedad unaPersona
    | ganarSubasta unaPersona = adquirirPropiedad unaPropiedad unaPersona
    | otherwise = unaPersona


adquirirPropiedad :: Propiedad -> Accion
adquirirPropiedad unaPropiedad unaPersona = unaPersona {dinero = (dinero unaPersona) - (precioPropiedad unaPropiedad), propiedad = propiedad unaPersona ++[unaPropiedad]}

propiedadBarata :: Propiedad -> Bool
propiedadBarata unaPropiedad = precioPropiedad unaPropiedad < 150

propiedadCara :: Propiedad -> Bool
propiedadCara unaPropiedad = not (precioPropiedad unaPropiedad < 150)

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

puedeComprarLaPropiedad :: Propiedad -> Persona -> Bool
puedeComprarLaPropiedad unaPropiedad unaPersona = (dinero unaPersona) >= (precioPropiedad unaPropiedad) 

hacerBerrinchePor :: Propiedad -> Accion
hacerBerrinchePor unaPropiedad unPersona
    | puedeComprarLaPropiedad unaPropiedad unPersona = adquirirPropiedad unaPropiedad unPersona
    | otherwise = (hacerBerrinchePor unaPropiedad).(sumarDinero 10).accionARealizar gritar $ unPersona 

ultimaRonda :: Persona-> Accion
ultimaRonda unaPersona= (foldr (.) id ).reverse.acciones $ unaPersona

juegoFinal :: Persona -> Persona -> Persona
juegoFinal persona1 persona2
    | dinero (ultimaRonda persona1 $ persona1) > dinero (ultimaRonda persona2 $ persona2) = ultimaRonda persona1 $ persona1
    | otherwise = ultimaRonda persona2 $ persona2

dineroUltimaRonda :: Persona->Int
dineroUltimaRonda unaPersona = dinero (ultimaRonda unaPersona $ unaPersona)

carolina :: Persona 
carolina = Persona "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Persona
manuel = Persona "Manuel" 500 "Oferente singular" [] [pasarPorElBanco,enojarse] 

{--Propiedades de prueba--}
mansion :: Propiedad
mansion = UnaPropiedad "Mansion" 550 

depto :: Propiedad
depto = UnaPropiedad "Departamento con vista al rio" 150 
<<<<<<< HEAD

choza :: Propiedad
choza =  UnaPropiedad "Choza humilde" 100
=======
>>>>>>> 5676c1f60c582d0f1e27902b5d7af22d5c5e1e81
