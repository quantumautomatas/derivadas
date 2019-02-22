{--
Programa para hacer automáticamente parte de la tarea 4
--}

-- Tipo que representa expresiones regulares
data Reg a = Void | Eps | Sym a | K (Reg a) | C (Reg a) (Reg a) | U (Reg a) (Reg a)
    deriving Show

-- Función de nulidad
v :: Reg a -> Reg a
v Void = Void
v Eps = Eps
v (Sym _) = Void
v (K _) = Eps
v (C  x y) = C (v x) (v y)
v (U x y ) = U (v x) (v y)

-- Derivada respecto a un símbolo
d :: (Eq a) => a -> Reg a -> Reg a
d _ Void = Void
d _ Eps = Void
d x (Sym y) = 
    if x == y
        then Eps
        else Void
d x (K y) = C (d x y) (K y)
d x (C y z) = U (C (d x y) z) (C (v y) (d x z))
d x (U y z) =  U (d x y) (d x z)

-- Derivada respecto a una cadena
ds :: (Eq a) => [a] -> Reg a -> Reg a
ds [] r = r
ds (x:xs) r = d x (ds xs r)

-- Función para simplificar uniones y concatenaciones que incluyan a la cadena 
-- vacía o a la expresión vacía
s:: Reg a -> Reg a
s Void = Void
s Eps = Eps
s (K Void) = Eps
s (C _ Void) = Void
s (C Void _) = Void
s (C Eps x) = x
s (C x Eps) = x
s (U x Void) = x
s (U Void x) = x
s (Sym x) = (Sym x)
s (K x) = (K (s x))
s (C x y) = (C (s x) (s y))
s (U x y) = (U (s x) (s y))

-- Función para simplificar n veces una expresión
sN :: Int -> Reg a -> Reg a
sN 0 r = r
sN n r = sN (n-1) (s r)

-- Expresión del inciso 2a
a :: Reg Char
a = (U 
        (K 
            (Sym 'a')
        )
        (K 
            (C 
                (K 
                    (Sym 'a')
                ) 
                (C 
                    (Sym 'b')
                    (C
                        (K 
                            (Sym 'a')
                        )
                        (C 
                            (Sym 'b')
                            (K 
                                (Sym 'a')
                            )
                        )
                    )
                )
            )
        )
    )

-- Respuesta del inciso 2a
ansA :: Reg Char
ansA = ds "bb" a

-- Expresión del inciso 2b
b :: Reg Char
b = (K 
        (C 
            (K 
                (Sym 'a') 
            ) 
            (C 
                (K 
                    (C 
                        (Sym 'b')
                        (C 
                            (Sym 'a') 
                            (Sym 'a')
                        ) 
                    ) 
                )
                (K 
                    (Sym 'a')
                )
            )
        )
    )

-- Respuesta del inciso 2b
ansB :: Reg Char
ansB = ds "ab" b

-- Expresión del inciso 2c
c :: Reg Char
c = (K 
        ( U 
            (C 
                (Sym 'a') 
                (Sym 'a') 
            ) 
            (C 
                (Sym 'b') 
                (Sym 'b') 
            ) 
        ) 
    )

-- Respuesta del inciso 2c
ansC :: Reg Char
ansC = ds "a" c
