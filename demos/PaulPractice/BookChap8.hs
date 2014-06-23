data T = TRUE |
         FALSE |
         If T T T |
         ZERO |
         Val Int |
         Pred T |
         IsZero T |
         Succ T |
         NUM_EXPECTED_ERROR |
         BOOL_EXPECTED_ERROR deriving Show


data TYPE = BOOL |
             NAT |
             ERROR deriving (Eq, Show)

getType :: T -> TYPE
getType TRUE = BOOL
getType FALSE = BOOL
getType (If x y z) = if ((getType x) == BOOL) then
                       if (getType y) == (getType z) then
                         getType y else
                         ERROR
                       else ERROR
getType (Pred t) = if (getType t) == NAT then NAT else ERROR
getType (Succ t) = if (getType t) == NAT then NAT else ERROR
getType (IsZero t) = if (getType t) == NAT then BOOL else ERROR
getType ZERO = NAT
getType Val{} = NAT
getType x = ERROR



         
eval :: T -> T

--eval (If TRUE y _) = eval y
eval (If x y z)
            | (getType x) /= ERROR =
                                   | x == TRUE = eval y
                                   | otherwise = eval z
            | otherwise = BOOL_EXPECTED_ERROR
eval (If x y z) = If (eval x) y z

eval (Succ TRUE) = NUM_EXPECTED_ERROR
eval (Succ FALSE) = NUM_EXPECTED_ERROR
eval (Succ x) = Succ (eval x)

eval (Pred ZERO) = ZERO
eval (Pred (Succ x)) = eval x
eval (Pred TRUE) = NUM_EXPECTED_ERROR
eval (Pred FALSE) = NUM_EXPECTED_ERROR
eval (Pred x) = Pred (eval x)

eval (IsZero ZERO) = TRUE
eval (IsZero (Succ x)) = FALSE
eval (IsZero TRUE) = NUM_EXPECTED_ERROR
eval (IsZero FALSE) = NUM_EXPECTED_ERROR
eval (IsZero x) = IsZero (eval x)
eval (x) = x
