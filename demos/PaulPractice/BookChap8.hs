module BookChap8 where

data T = TRUE 
       | FALSE 
       | If T T T
       | ZERO
       | Pred T
       | IsZero T
       | Succ T deriving (Eq, Show)


data TYPE = BOOL
          | NAT deriving (Eq, Show)

getType :: T -> Either String TYPE
getType TRUE = Right BOOL
getType FALSE = Right BOOL
getType (If x y z) =
    do x' <- getType x
       y' <- getType y
       z' <- getType z
       if x' == BOOL && y' == z'
          then return y'
          else Left "Bad Type: conditional"
getType (Pred t) 
    do t' <- getType t
       if t' == NAT
          then return NAT
          else Left "Bad Type: pred."
getType (Succ t) 
    do t' <- getType t
       if t' == NAT
          then return NAT
          else Left "Bad Type: succ."
getType (IsZero t) 
    do t' <- getType t
       if t' == NAT
          then return BOOL
          else Left "Bad Type: zero?."
getType ZERO = NAT


eval :: T -> T
eval (If TRUE y _) =  eval y 
eval (If FALSE _ z) = eval z
eval (If x y z) = eval $ If (eval x) y z
eval (Succ ZERO) = Val 1
eval (Succ x) = 
    let (Val x') = eval x in
      Val $ x' + 1
eval (Pred ZERO) = ZERO
eval (IsZero ZERO) = TRUE
eval (IsZero Succ{}) = FALSE
eval (IsZero x) = IsZero $ eval x
eval x = x

run :: T -> T
run tm =
    case getType tm of
      Right _ -> eval tm
      Left str -> error str

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
