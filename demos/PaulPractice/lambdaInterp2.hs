data T = Var Char |
         Abst T T |
         App T T deriving (Eq, Show)



eval :: T -> T
eval x = evalHelper (fst (fixNameConflicts x []))


evalHelper :: T->T
evalHelper (App (Abst x y) z) = evalHelper (sub y x z)
evalHelper (Abst x (App a b)) = Abst x (evalHelper (App (evalHelper a) (evalHelper b)))
evalHelper (App x y) = evalHelper (App (evalHelper x) (evalHelper y))
evalHelper x = x


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

fixNameConflicts :: T -> [Char] -> (T,[Char])
fixNameConflicts (App x y) takens = case (fixNameConflicts x takens) of 
                                    (tNew, takenNew) -> case (fixNameConflicts y takenNew) of
                                                        (tNew2, takenNew2) -> ((App tNew tNew2), takenNew2) 
fixNameConflicts (Abst (Var x) bod) takens = if x `elem` takens then 
                                               case (rename (Abst (Var x) bod) takens) of
                                               (tNew, takenNew) -> fixNameConflicts tNew takenNew
                                             else case (fixNameConflicts bod (x : takens)) of
                                                   (tNew, takenNew) -> ( (Abst (Var x) tNew), takenNew)
fixNameConflicts x takens = (x,takens)

--perform rename and add to takens
rename :: T -> [Char] -> (T,[Char])
rename (Abst (Var x) bod) takens = case (getNewChar x takens) of
                                   (n) -> (Abst (Var n) (renameHelper bod (Var x) (Var n)), (x : takens))
rename x y = (x, y)

--in T1 rename T2 to T3
renameHelper :: T -> T -> T -> T
renameHelper (App (Abst x y) z) old new = App (Abst x y) (renameHelper z old new)
renameHelper (App x y) old new  = App (renameHelper x old new) (renameHelper y old new)
renameHelper (Abst x y) old new = if y == old then Abst x new else Abst x (renameHelper y old new)
renameHelper x old new = if x == old then new else x


getNewChar :: Char -> [Char] -> Char
getNewChar x takens = head [ x | x <- ['A'..'Z'], not (x `elem` takens)]
