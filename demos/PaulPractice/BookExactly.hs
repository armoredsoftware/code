module BookExactly
    ( T(..)
    , V(..)
    , eval
    , simplify
    , eval2
    ) where

data T = TRUE |
         FALSE |
         If T T T

data V = TRUEv |
         FALSEv deriving Show

eval :: T->V
eval TRUE = TRUEv
eval FALSE = FALSEv
eval (If TRUE y z) = eval y
eval (If FALSE y z) = eval z
eval (If x y z) = eval (If (simplify x) y z)

simplify :: T -> T
simplify TRUE = TRUE
simplify FALSE = FALSE
simplify (If TRUE y z) = y
simplify (If FALSE y z) = z
simplify (If x y z) = If (simplify x) y z

{- Alternatively, if separate term and value spaces aren't too complex, we can
   combine them.
-}
eval2 :: T -> T
eval2 (If TRUE y _) = eval2 y
eval2 (If FALSE _ z) = eval2 z
eval2 (If x y z) = If (eval2 x) y z
eval2 x = x
