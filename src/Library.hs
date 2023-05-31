module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Promedio = Number
type Sabor = String
data Postre = UnPostre {
    sabores :: [Sabor],
    peso :: Number,
    temperatura:: Number
} deriving Show

type Hechizo = Postre -> Postre

flan :: Postre
flan = UnPostre {
    sabores = ["ascas"],
    peso= 500,
    temperatura = 10
}

incendio :: Hechizo
incendio postre = postre {
    temperatura = temperatura postre + 1,
    peso = peso postre * 0.95
}

immobulus :: Hechizo
immobulus postre = postre {
    temperatura = 0
}

wingardium :: Hechizo
wingardium postre = postre {
    sabores = sabores postre ++ ["concentrado"],
    peso = peso postre * 0.90
}

diffindo:: Number -> Hechizo
diffindo porcentaje postre = postre {
    peso = peso postre * ((100 - porcentaje)/100)
}

riddikulus:: Sabor -> Hechizo
riddikulus sabor postre = postre {
    sabores = sabores postre ++ [reverse sabor]
}

vaciarSabores:: Hechizo
vaciarSabores postre = postre { sabores = []}

avada:: Hechizo
avada = immobulus . vaciarSabores


-- Punto C

pesaMasQueCero:: Postre -> Bool
pesaMasQueCero postre = peso postre > 0

tieneAlgunSabor:: Postre -> Bool
tieneAlgunSabor postre = length (sabores postre) > 0

noEstaCongelado:: Postre -> Bool
noEstaCongelado postre = temperatura postre > 0

estaPostreListo:: Postre -> Bool
estaPostreListo postre = pesaMasQueCero postre &&  tieneAlgunSabor postre &&  noEstaCongelado postre

saberSiQuedanListos:: Hechizo -> [Postre] -> Bool
saberSiQuedanListos hechizo = all (estaPostreListo . hechizo)

-- Punto D
-- Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos. 
promedioPesoPostresListos:: [Postre] -> Promedio
promedioPesoPostresListos postres = obtenerPromedioPeso (filter estaPostreListo postres)

obtenerPesoPostre:: Postre -> Number
obtenerPesoPostre  = peso

obtenerPromedioPeso:: [Postre] -> Promedio
obtenerPromedioPeso postres = sum (map obtenerPesoPostre postres) / length postres

-- Punto 2A
-- Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un hechizo sobre un postre (se espera obtener el mago).
-- Cuando un mago practica con un hechizo, lo agrega a sus hechizos aprendidos. 
-- Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada kedavra” al postre, entonces suma un horrorcrux.

data Mago = UnMago {
    hechizosAprendidos:: [Hechizo],
    cantidadDeHorrocruxes:: Number
}deriving Show

hp = UnMago {
    hechizosAprendidos = [],
    cantidadDeHorrocruxes = 0
}


aprenderHechizo:: Hechizo -> Mago -> Mago
aprenderHechizo hechizo mago = mago { hechizosAprendidos = hechizosAprendidos mago ++ [hechizo]}

sonPostresIguales:: Postre -> Postre -> Bool
sonPostresIguales postre1 postre2 = peso postre1 == peso postre2 &&  temperatura postre1 == temperatura postre2 && sabores postre1 == sabores postre2

sumarONoHorrocrux:: Hechizo -> Postre -> Mago -> Mago
sumarONoHorrocrux hechizo postre mago
    | sonPostresIguales (hechizo postre) (avada postre) = mago { cantidadDeHorrocruxes = cantidadDeHorrocruxes mago + 1}
    | otherwise = mago

practicarHechizo:: Hechizo -> Postre -> Mago -> Mago
practicarHechizo hechizo postre  = aprenderHechizo hechizo . sumarONoHorrocrux hechizo postre

-- Punto 2B
-- Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al postre con más cantidad de sabores luego de usarlo.

--Lo saque de la resolucion porque no lo entendia

mejorHechizoV1 :: Postre -> Mago -> Hechizo
mejorHechizoV1 postre mago = elMejor postre (hechizosAprendidos mago)

elMejor :: Postre -> [Hechizo] -> Hechizo
elMejor postre [hechizo] = hechizo
elMejor postre (primer:segundo:restohechizos) | esMejor postre primer segundo = elMejor postre (primer:restohechizos)
    | otherwise = elMejor postre (segundo:restohechizos)

esMejor :: Postre -> Hechizo -> Hechizo -> Bool
esMejor postre hechizo1 hechizo2 = (length . sabores . hechizo1) postre > (length . sabores . hechizo2) postre


-- Punto 3A

obtenerListaDeAlgo:: a -> [a]
obtenerListaDeAlgo a = a : obtenerListaDeAlgo a

obtenerListaDePostres:: Postre -> [Postre]
obtenerListaDePostres = obtenerListaDeAlgo

obtenerMagoConInfinitosPoderes:: Mago -> Hechizo -> Mago
obtenerMagoConInfinitosPoderes mago hechizo = mago { hechizosAprendidos = obtenerListaDeAlgo hechizo }

-- Punto 3B
-- Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos
-- ¿Existe alguna consulta que pueda hacer para que me sepa dar una respuesta? Justificar conceptualmente.

-- Se podría ir preguntando uno a uno si los deja listos sin problema, ya que haskel va mostrando el resultado a medida que avanza la lista. Sería una
-- lista infinita de True o False.


-- Punto 3C 
-- Suponiendo que un mago tiene infinitos hechizos
-- ¿Existe algún caso en el que se puede encontrar al mejor hechizo? Justificar conceptualmente.

-- Es imposible encontrar el mejor hechizo porque lo deberia comprar contra todos los demas, y al ser una lista infinita nunca podria terminar de comprararlos.