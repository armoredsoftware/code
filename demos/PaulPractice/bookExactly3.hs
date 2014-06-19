data T = TRUE |
         FALSE |
         If T T T |
         ZERO |
         Pred T |
         IsZero T |
         Succ T |
         NUM_EXPECTED_ERROR |
         BOOL_EXPECTED_ERROR deriving Show


eval :: T -> T

eval (If TRUE y _) = eval y
eval (If FALSE _ z) = eval z
eval (If ZERO _ _) = BOOL_EXPECTED_ERROR
eval (If (Succ _) _ _) = BOOL_EXPECTED_ERROR
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
         
