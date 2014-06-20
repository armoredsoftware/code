module SimpleParser where

{--Informally, commands c are described by the following BNF grammar:
 
     c ::= SKIP
         | x ::= a
         | c ;; c
         | WHILE b DO c END
         | IFB b THEN c ELSE c FI
 
 
For example, here's the factorial function in Imp.
 
     Z ::= X;;
     Y ::= 1;;
     WHILE not (Z = 0) DO
       Y ::= Y × Z;;
       Z ::= Z - 1
     END
 
When this command terminates, the variable Y will contain the factorial of the initial value of X.
        --}
data A 
    = Val Int
    | A `Plus` A
    | A `Minus` A
    | A `Times` A deriving Show
data  B 
    = TRUE | FALSE
    | A `Eq` A
    | A `LtEq` A
    | Not B 
    | B `And` B

data END = ENDc
data WHILE = WHILEc
data DO = DOc
data IFB = IFBc
data THEN = THENc
data ELSE = ELSEc
data FI = FIc

data C = SKIP 
         | Var `Setto` A
         | WHILE B DO C END
         | IFB B THEN C ELSE FI
         | C `EL` C
         
         
         
 

data Var = Q | R | S | T | U | V | W | X | Y | Z deriving Show

interp :: C -> Int  -> Int
interp ((x `Setto` y) `EL` nextc) _ = let x = (eval y) in interp nextc x 
interp (SKIP `EL` nextc) c = interp nextc c
interp (WHILE x DOc y ENDc `EL` nextc) c = if (eval2 x) then ((interp (WHILE x DOc y ENDc `EL` nextc) (interp y c))) else interp nextc c
interp _ c = c
 

          
eval :: A -> Int 
eval (Val x) = x
eval (x `Plus` y) = (eval x) + (eval y)
eval (x `Minus` y) = (eval x) - (eval y)
eval (x `Times` y) = (eval x) * (eval y)



{-- test data:
        eval $ (Val 7) `Plus` (Val 8) `Minus` (Val 2) `Times` (Val 3)

interp ((X `Setto` (Val 3)) `EL` (WHILE (X `LtEq` (Val 4)) (X `Setto` (Val (X + (Val 1)))) ENDc) `EL` SKIP) 0


interp (X `Setto` (Val 3 `Plus` Val 4) `EL` SKIP) 0

a proper B : ((Val 3) `Eq` (Val 4))

works : interp (WHILE ((Val 3) `Eq` (Val 4)) DOc (X `Setto` (Val 3 `Plus` Val 4)) ENDc `EL` SKIP) 0
works: interp ((X `Setto` (Val 3 `Plus` Val 4)) `EL` WHILE ((Val 3) `Eq` (Val 4)) DOc (X `Setto` (Val 3 `Plus` Val 4)) ENDc `EL` SKIP) 0
--}



eval2 :: B -> Bool
eval2 TRUE = True
eval2 FALSE = False
eval2 (x `Eq` y) = (eval x) == (eval y)
eval2 (x `LtEq` y) = (eval x) <= (eval y)
eval2 (Not x) = not (eval2 x)
eval2 (x `And` y) = and [(eval2 x), (eval2 y)]

