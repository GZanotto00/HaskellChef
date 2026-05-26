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
esMayorA10Gramos = (> 10).pesoEnGramos

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

-------------------------------------------------

especialDePepeRonccino :: Plato
especialDePepeRonccino = UnPlato {
    nombrePlato = "Especial Pepe",
    dificultad = 8,
    componentes = [(UnComponente "leche" 1000), (UnComponente "manteca" 100), (UnComponente "semola" 250), (UnComponente "queso" 50), (UnComponente "pimienta" 3), (UnComponente "nuez moscada" 1)]
}

pepeRonccino :: Participante
pepeRonccino = UnParticipante {
    nombreParticipante = "Pepe Ronccino",
    trucos = [(darSabor 2 5), simplificar, duplicarPorcion],
    platoEspecial = especialDePepeRonccino
}

-----------------------------------------------

aplicarTruco :: Truco -> Plato -> Plato
aplicarTruco truco plato = truco plato

aplicarTrucosAPlato :: Participante -> Plato
aplicarTrucosAPlato participante = foldr aplicarTruco (platoEspecial participante) (trucos participante)

cocinar :: Participante -> Plato
cocinar participante = aplicarTrucosAPlato participante

----------------------------------------------------

tieneMayorDificultad :: Plato -> Plato -> Bool
tieneMayorDificultad plato1 plato2 = dificultad plato1 > dificultad plato2

sumarPesoComponente :: Componente -> Number -> Number
sumarPesoComponente componente peso = peso + (pesoEnGramos componente)

pesoPlato :: Plato -> Number
pesoPlato plato = foldr sumarPesoComponente 0 (componentes plato)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue plato1 plato2 = (plato1 `tieneMayorDificultad` plato2) && (pesoPlato plato1) < (pesoPlato plato2)

----------------------------------------------------

tieneMejorPlatoCocinado :: Participante -> Participante -> Bool
tieneMejorPlatoCocinado participante1 participante2 = cocinar participante1 `esMejorQue` cocinar participante2

participanteConMejorPlatoCocinado :: Participante -> Participante -> Participante
participanteConMejorPlatoCocinado participante1 participante2
  | participante1 `tieneMejorPlatoCocinado` participante2 = participante1
  | otherwise = participante2

participanteEstrella :: [Participante] -> Participante
participanteEstrella [p] = p
participanteEstrella (x:xs) = participanteConMejorPlatoCocinado x (participanteEstrella xs)
