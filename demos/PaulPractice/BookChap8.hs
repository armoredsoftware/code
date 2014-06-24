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
getType (If x y z) = if getType x == BOOL && getType y == getType z then
                       getType y
                     else
                       ERROR

getType (Pred t) = if getType t == NAT then NAT else ERROR
getType (Succ t) = if getType t == NAT then NAT else ERROR
getType (IsZero t) = if getType t == NAT then BOOL else ERROR
getType ZERO = NAT
getType Val{} = NAT
getType x = ERROR



eval :: T -> T
eval (If x y z) | getType x ==BOOL = case x of
                                          (TRUE)  -> eval y
                                          (FALSE) -> eval z
                                          (_)     -> eval (If (eval x) y z)
                | otherwise = BOOL_EXPECTED_ERROR
eval (Succ x)
            | getType x == NAT = case x of
                                 (ZERO)  -> ZERO
                                 (Val v) -> Val (v+1)
                                 (_)     -> eval (Succ (eval x))
            | otherwise = NUM_EXPECTED_ERROR
eval (Pred x)
            | getType x ==NAT = case x of
                                        (ZERO)  -> Val 1
                                        (Val 1) -> ZERO
                                        (Val v) -> Val (v-1)
                                        (_)     -> eval (Pred (eval x))
            | otherwise = NUM_EXPECTED_ERROR
eval (IsZero x)
              | getType x == NAT = case x of
                                             (ZERO) -> TRUE
                                             (Val v)-> FALSE
                                             (_)    -> eval (IsZero (eval x))
              | otherwise = NUM_EXPECTED_ERROR
eval (x) = x


{-- Test data
*Main> eval (If (If TRUE (Val 3) (Val 4)) (Val 9) (Val 20))
BOOL_EXPECTED_ERROR
*Main> eval (If (If TRUE (FALSE) (TRUE)) (Val 9) (Val 20))
Val 20

*Main> eval (Pred (Val 1))
ZERO
*Main> getType (If TRUE TRUE TRUE)
BOOL
*Main> getType (If (Val 3) TRUE TRUE)
ERROR
*Main> getType (If (TRUE) (Val 3)  TRUE)
ERROR
*Main> getType (If (TRUE) (Val 3)  (Val 4))
NAT

--}
