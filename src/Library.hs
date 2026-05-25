module Library where
import PdePreludat

data Participante = UnParticipante {
    nombreParticipante :: String,
    trucos :: [Truco],
    platoEspecial :: Plato
} deriving (Show)

data Plato = UnPlato {
    nombrePlato :: String,
    dificultad :: Number,
    componentes :: [Componente]
} deriving (Show)

data Componente = UnComponente {
    ingrediente :: String,
    pesoEnGramos :: Number
} deriving (Show)

oregano :: Componente
oregano = UnComponente {
    ingrediente = "sal",
    pesoEnGramos = 4
}

aceite :: Componente
aceite = UnComponente {
    ingrediente = "aceite",
    pesoEnGramos = 57
}

platoEj :: Plato
platoEj = UnPlato {
    nombrePlato = "Prueba",
    dificultad = 26,
    componentes = [aceite, oregano, aceite, aceite, aceite, oregano]
}


type Truco = Plato -> Plato

modificarComponentes :: ([Componente] -> [Componente]) -> Plato -> Plato
modificarComponentes accionComponentes plato = plato { componentes = accionComponentes (componentes plato) }

agregarComponente :: Componente -> Truco
agregarComponente componente = modificarComponentes (componente :) 

agregarAzucar :: Number -> Truco
agregarAzucar cantidad = agregarComponente (UnComponente "azucar" cantidad)

agregarSal :: Number -> Truco
agregarSal cantidad = agregarComponente (UnComponente "sal" cantidad)

endulzar :: Number -> Truco
endulzar cantidadAzucar = agregarAzucar cantidadAzucar

salar :: Number -> Truco
salar cantidadSal = agregarSal cantidadSal

darSabor :: Number -> Number -> Truco
darSabor cantidadSal cantidadAzucar = (endulzar cantidadAzucar).(salar cantidadSal)

---------------------------------------

modificarPeso :: (Number -> Number) -> Componente -> Componente
modificarPeso accionPeso componente = componente { pesoEnGramos = accionPeso (pesoEnGramos componente) }

duplicarPeso :: Componente -> Componente
duplicarPeso componente = modificarPeso (* 2) componente

duplicarPorcion :: Truco
duplicarPorcion = modificarComponentes (map duplicarPeso)


---------------------------------------

tieneMasDe5Componentes :: Plato -> Bool
tieneMasDe5Componentes plato = (> 5).length.componentes $ plato

dificultadMayorA7 :: Plato -> Bool
dificultadMayorA7 plato = (> 7).dificultad $ plato

modificarDificultad :: (Number -> Number) -> Plato -> Plato
modificarDificultad accionDificultad plato = plato { dificultad = accionDificultad (dificultad plato) }

esMayorA10Gramos :: Componente -> Bool
esMayorA10Gramos = (< 10).pesoEnGramos

filtrarComponentesDesde10Gramos :: [Componente] -> [Componente]
filtrarComponentesDesde10Gramos = filter esMayorA10Gramos

simplificar :: Truco
simplificar plato
  | tieneMasDe5Componentes plato && dificultadMayorA7 plato = (modificarDificultad (const 5)).(modificarComponentes filtrarComponentesDesde10Gramos) $ plato
  | otherwise = plato

------------------------------------

type CaracteristicaPlato = Plato -> Bool

esIngrediente :: String -> Componente -> Bool
esIngrediente unIngrediente componente = unIngrediente == ingrediente componente

tieneIngrediente :: String -> CaracteristicaPlato
tieneIngrediente ingrediente plato = any (esIngrediente ingrediente) (componentes plato)

alimentosLacteos :: [String]
alimentosLacteos = ["leche", "manteca", "crema", "queso"]

esAlimentoLacteo :: Componente -> Bool
esAlimentoLacteo componente = elem (ingrediente componente) alimentosLacteos

tieneIngredientesLacteos :: CaracteristicaPlato
tieneIngredientesLacteos = any esAlimentoLacteo . componentes

esVegano :: CaracteristicaPlato
esVegano plato = not (tieneIngrediente "carne" plato || tieneIngrediente "huevos" plato || tieneIngredientesLacteos plato)
    
esSinTacc :: CaracteristicaPlato
esSinTacc = not.(tieneIngrediente "harina")

esComplejo :: CaracteristicaPlato
esComplejo plato = tieneMasDe5Componentes plato && dificultadMayorA7 plato

masDe2GramosDeSal :: Componente -> Bool
masDe2GramosDeSal componente = esIngrediente "sal" componente && ((> 2).pesoEnGramos $ componente)

noAptoHipertension :: CaracteristicaPlato
noAptoHipertension plato = any masDe2GramosDeSal (componentes plato)