module LambdaInterp where

data T = Var Char |
         Abst T T |
         App T T deriving (Eq, Show)


{- ECA:  This is a "large-step" semantics matching with the book's full 
         beta-redex semantics.  The actual rules are for a "small-step",
         call-by-value semantics, though.
-}
eval :: T -> T
eval (App (Abst x y) z) = eval (sub y x z)
eval (Abst x (App a b)) = Abst x (eval (App (eval a) (eval b)))
eval (App x y) = eval (App (eval x) (eval y))
eval x = x

-- in T replace T with T yields T
{- ECA:  Think about how we might represent a substitution environment, and use
         that to decide where to curry/uncurry arguments.  For example, if we
         represetn a substitution environment as an association list, we can
         write the type of sub as T -> (T, T) -> T.  This will let us do stuff
         like map (sub z) env if we need to do more than one substitution at a
         time!
-}
sub :: T -> T -> T -> T
sub (Var x) (Var y) z = if x ==y then z else Var x
sub (Abst (Var a) x) (Var y) z = Abst (Var a) (sub x (Var y) z)
sub (App a b) (Var y) z = App (sub a (Var y) z) (sub b (Var y) z)

{- ECA:  Look at how much closer this implementation matches our written rules.
         Note that ordering of the conditionals matters!
-}
eval2 :: T -> T
eval2 (App x y)
    | isAbst x && isValue y =            -- E-AppAbs
          let (Abst a z) = x in
            sub2 z (y, a) 
    | isValue x = eval . App x $ eval y  -- E-App2
    | otherwise = eval $ App (eval x) y  -- E-App1
eval2 x = x

isAbst :: T -> Bool
isAbst Abst{} = True
isAbst _ = False

isValue :: T -> Bool
isValue Abst{} = True
isValue Var {} = True
isValue _ = False

{- ECA:  Note how much cleaner this looks for recursive calls. -}
sub2 :: T -> (T, T) -> T
sub2 z (Var x, Var y)
    | x == y = z
    | otherwise = Var x
sub2 (Abst a x) env =
    Abst a $ sub2 x env
sub2 (App a b) env =
    App (sub2 a env) $ sub2 b env
sub2 _ _ = error "sub2: bad substitution pair."

-- ECA: Here's alpha-equivalence if you're curious
alphaEq :: T -> T -> Bool
alphaEq = orda []
    where orda :: [(T, T)] -> T -> T -> Bool
          orda env tm1 tm2
              | tm1 == tm2 && all (uncurry (==)) env = True
              | otherwise =
                  case (tm1, tm2) of
                    (Var{}, Var{}) -> ordav env tm1 tm2
                    (App s1 t1, App s2 t2) ->
                        orda env s1 s2 && orda env t1 t2
                    (Abst x1@Var{} t1, Abst x2@Var{} t2) ->
                        orda ((x1, x2):env) t1 t2
                    _ -> False

          ordav :: [(T, T)] -> T -> T -> Bool
          ordav [] x1 x2 = x1 == x2
          ordav ((l, r):oenv) x1 x2
              | x1 == l = x2 == r
              | otherwise = ordav oenv x1 x2


tru =  Abst (Var 't') (Abst (Var 'f') (Var 't'))
fls = Abst (Var 't') (Abst (Var 'f') (Var 'f'))
and' = Abst (Var 'b') (Abst (Var 'c') (App (App (Var 'b') (Var 'c')) fls))


tru2 = Abst (Var 'a') (Abst (Var 'b') (Var 'a'))

test = alphaEq tru tru2



{-test data
tru == eval2 (App( App and' tru) fls)
tru == eval2 (App( App and' tru) tru)
tru == eval2 (App( App and' fls) tru)
-}
