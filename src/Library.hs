module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

type Material = String
data Personaje = UnPersonaje {
    nombre:: String,
    puntaje:: Number,
    inventario:: [Material]
} deriving Show
type Tiempo = Number

data Receta = UnaReceta {
    materiales:: [Material],
    tiempo :: Tiempo,
    nombreReceta :: String
} deriving Show

fogata :: Receta
fogata = UnaReceta {
    materiales= ["madera","fosforo"],
    tiempo = 10,
    nombreReceta = "fogata"
}

pollo :: Receta
pollo = UnaReceta {
    materiales= ["pollo","fogata"],
    tiempo = 300,
    nombreReceta = "pollo"
}

steve:: Personaje
steve = UnPersonaje {
    nombre= "hola",
    puntaje= 1000,
    inventario  = ["fogata"]
}

craftearReceta:: Receta -> Personaje -> Personaje
craftearReceta receta unPersonaje
    | tieneLosMateriales receta unPersonaje = (actualizarPuntaje receta . sacarMateriales receta . agregarNuevoMaterial (nombreReceta receta) ) unPersonaje
    | otherwise = unPersonaje { puntaje = puntaje unPersonaje - 100 }

sacarMateriales:: Receta -> Personaje -> Personaje
sacarMateriales receta unPersonaje = unPersonaje { inventario = quitarMaterialesDelInventario receta (inventario unPersonaje)}

--Por ahora, quita todas las unidades de los materiales, aunque solo deberia quitarlos una vez
quitarMaterialesDelInventario:: Receta -> [Material] -> [Material]
quitarMaterialesDelInventario receta = filter (\mat -> mat `notElem` materiales receta)

tieneLosMateriales :: Receta -> Personaje -> Bool
tieneLosMateriales receta unPersonaje = all (\a -> a `elem` inventario unPersonaje) (materiales receta)

agregarNuevoMaterial:: Material -> Personaje -> Personaje
agregarNuevoMaterial material unPersonaje = unPersonaje { inventario = inventario unPersonaje ++ [material]}

actualizarPuntaje:: Receta-> Personaje-> Personaje
actualizarPuntaje receta unPersonaje = unPersonaje { puntaje = puntaje unPersonaje + 10 * tiempo receta}

duplicaPuntaje:: Receta -> Personaje -> Bool
duplicaPuntaje receta unPersonaje = puntaje unPersonaje + 10 * tiempo receta >= 2 * puntaje unPersonaje

obtenerCrafteables:: [Receta] -> Personaje -> [String]
obtenerCrafteables recetas unPersonaje = map nombreReceta (filter (\unaReceta -> tieneLosMateriales unaReceta unPersonaje && duplicaPuntaje unaReceta unPersonaje) recetas)

craftearListaRecetas :: [Receta] -> Personaje ->Personaje
craftearListaRecetas recetas unPersonaje = foldl (\pers receta -> craftearReceta receta pers) unPersonaje recetas

-- 2c no lo hice

-- Mine 
data Bioma = UnBioma {
    materialNecesario :: Material,
    materialesBioma :: [Material]
} deriving Show

artico :: Bioma
artico = UnBioma {
    materialNecesario = "sueter",
    materialesBioma = ["hielo","iglu", "lobo"]
}

type Herramienta = [Material] -> Material

hacha :: Herramienta
hacha = last

espada :: Herramienta
espada = head 

pico :: Number -> Herramienta
pico pos mats = mats !! pos  


tieneMaterialNecesario:: Material -> Personaje ->Bool
tieneMaterialNecesario mat unPersonaje = elem mat (inventario unPersonaje)

sumarPuntos:: Number -> Personaje -> Personaje
sumarPuntos puntos unPersonaje = unPersonaje { puntaje = puntaje unPersonaje + puntos}
minar:: Herramienta -> Personaje -> Bioma -> Personaje
minar unaHerramienta unPersonaje unBioma
    | tieneMaterialNecesario (materialNecesario unBioma) unPersonaje = (agregarNuevoMaterial (unaHerramienta (materialesBioma unBioma)) . sumarPuntos 50) unPersonaje
    | otherwise = unPersonaje

-- me falta el 2 y el 3
