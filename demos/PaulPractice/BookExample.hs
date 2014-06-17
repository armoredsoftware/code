module BookExample where


--data T = TRUE | FALSE | Cond T T T

data ExprT = TRUE |
             FALSE |
             Cond ExprT ExprT ExprT |
             ExprT `AND` ExprT |
             Not ExprT |
             ExprT `OR` ExprT |
             ExprT `XOR` ExprT |
             Succ ExprT |
             Pred ExprT |
             IsZero ExprT |
             ZERO |
             ERROR deriving (Eq, Show)
             


eval :: ExprT -> ExprT
eval TRUE = TRUE
eval FALSE= FALSE
eval (Not x) = if (eval x) == TRUE then FALSE else TRUE
eval (Cond x y z) | (eval x)==TRUE = eval y
                  | otherwise = eval z
eval (x `AND` y) =if (eval x) ==TRUE  && (eval y) ==TRUE then TRUE else FALSE
eval (x `OR` y) = if (eval x) ==TRUE || (eval y) == TRUE then TRUE else FALSE
eval (x `XOR` y) =if ((eval x)==TRUE) && (eval (Not y))==TRUE || ((eval y)==TRUE) && (eval (Not x))==TRUE then TRUE else FALSE
eval (Succ x) = Succ (eval x)
eval (Pred ZERO) = ZERO
eval (Pred (Succ x))= x
eval (Pred x) = Pred (eval x)
eval (IsZero ZERO) = TRUE
eval (IsZero (Succ x))= FALSE
eval (IsZero x) = (IsZero (eval x))
eval _ = ERROR


{-- TEST DATA
eval (Cond FALSE FALSE TRUE)
eval (Cond (FALSE `XOR` TRUE) TRUE FALSE)

eval (Not TRUE)
eval (Succ ZERO)
eval (Not (Not TRUE))
--}

