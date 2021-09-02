module Library where
import PdePreludat

-- tuplas
-- permiten que sus componentes sean de distinto tipo (mientras que las listas no).
-- no se pueden agregar nuevos valores como en las listas.
perroTupla = ("Firulais", 5)
---

data Perro = UnPerro String String Number
    deriving (Show, Eq)

perroData = UnPerro "Firulais" "boxer" 5

cumplir1Anio (UnPerro nombre raza edad) = UnPerro nombre raza (edad + 1) 

nombre (UnPerro nombre _ _) = nombre
raza (UnPerro _ raza _) = raza
edad (UnPerro _ _ edad) = edad

--- 

data PerroRecord = UnPerroR { nombreR :: String, razaR :: String, edadR :: Number }
    deriving (Show, Eq)

esNombreLargo unPerro = length (nombreR unPerro) > 4 

data Alumno = UnAlumno { nombreAlumno :: String, legajo :: Number,  materias :: [String]}
    deriving (Show)

pablo = UnAlumno "Pablo" 214123 ["matematica discreta", "fisica 2", "quimica", "pdp"]

inscribirAMateria (UnAlumno nombre legajo materias) materia = UnAlumno nombre legajo (materia : materias)

-- forma mas abreviada
inscribirAMateria2 alumno materiaNueva = alumno { materias = (materiaNueva : materias alumno) }


data FormaGeometrica = Cuadrado { lado :: Number } 
                     | Circulo { radio :: Number }
                     | Rectangulo Number Number -- base y altura
                     | Triangulo (Number, Number, Number) -- lado1, lado2, lado3
    deriving (Show, Eq, Ord)

area (Cuadrado lado) = lado ^ 2
area (Circulo radio) = 3.1415 * radio ^ 2
area (Rectangulo base altura) = base * altura  

esIgualOMayor f1 f2 = (f1 == f2) || (f1 > f2)

esGrande :: FormaGeometrica -> Bool
esGrande forma = area forma > 100

-- 2 + 3

-- ( 2 + 3) + 2
data ExpresionAritmetica = LiteralNumerico Number
                         | Suma ExpresionAritmetica ExpresionAritmetica
                         | Multiplicacion ExpresionAritmetica ExpresionAritmetica
                         | Variable String Number
    deriving Show

-- 2 + 3
dosMastres = Suma (LiteralNumerico 2) (LiteralNumerico 3)

evaluar (LiteralNumerico x) = x
evaluar (Suma expr1 expr2) = (evaluar expr1) + (evaluar expr2)
evaluar (Multiplicacion expr1 expr2) = (evaluar expr1) * (evaluar expr2)
evaluar (Variable _ valor) = valor


