data T = Var Char |
         Abst T T |
         App T T deriving (Eq, Show)



eval :: T -> T
eval (App (Abst x y) z) = eval (sub y x z)
eval (Abst x (App a b)) = Abst x (eval (App (eval a) (eval b)))
eval (App x y) = eval (App (eval x) (eval y))
eval x = x


-- in T replace T with T yields T
sub :: T -> T -> T -> T
sub (Var x) (Var y) z = if x ==y then z else Var x
sub (Abst (Var a) x) (Var y) z = Abst (Var a) (sub x (Var y) z)
sub (App a b) (Var y) z = App (sub a (Var y) z) (sub b (Var y) z)



tru =  Abst (Var 't') (Abst (Var 'f') (Var 't'))
fls = Abst (Var 't') (Abst (Var 'f') (Var 'f'))
and' = Abst (Var 'b') (Abst (Var 'c') (App (App (Var 'b') (Var 'c')) fls))



{-test data
tru == eval (App( App and' tru) fls)
tru == eval (App( App and' tru) tru)
tru == eval (App( App and' fls) tru)
-}
