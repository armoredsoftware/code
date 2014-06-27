module BookSimpleTypedLambda where

data T = Var Char |
         Varj Int |
         Abst T T |
         App T T deriving (Eq)


{- ECA:  This is a "large-step" semantics matching with the book's full 
         beta-redex semantics.  The actual rules are for a "small-step",
         call-by-value semantics, though.
-}
eval :: T -> T
eval (App (Abst x y) z) = eval (sub y x z)
eval (Abst x y) = Abst x (eval y)
eval (App x y) | isValue x = App x (eval y)
               | otherwise =eval (App (eval x) (eval y))
{--
eval (App (Var x) y) = App (Var x) (eval y)
eval (App (Varj x) y) = App (Varj x) (eval y)
eval (App x y) =  (App (eval x) y)
--}
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
sub (Abst (Var a) x) (Var y) z = Abst (Var a) (sub x (Var y) z)
sub (App a b)  y z = App (sub a y z) (sub b y z)
sub x (Varj i) z = subDBHelper x 1 z
sub x y z = if x==y then z else x

--In T, replace Varj Int with T
subDBHelper :: T -> Int -> T -> T
subDBHelper (Abst x y) i new = Abst x (subDBHelper y (i +1) new)
subDBHelper (App a b) i new = App (subDBHelper a i new) (subDBHelper b i new)
subDBHelper x i z = if x == (Varj i) then z else x 
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
isValue Var {} = True
isValue Varj{} = True
isValue (App a b) = isValue a && isValue b
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


tru =   (Abst (Var 't') (Abst (Var 'f') (Var 't')))
fls =  (Abst (Var 't') (Abst (Var 'f') (Var 'f')))
and' = ( Abst (Var 'b') (Abst (Var 'c') (App (App (Var 'b') (Var 'c')) fls)))


tru2 = Abst (Var 'a') (Abst (Var 'b') (Var 'a'))

test = alphaEq tru tru2



{-test data
tru == eval (App( App and' tru) fls)
tru == eval (App( App and' tru) tru)
tru == eval (App( App and' fls) tru)
-}

data Lambda = Lambda String

instance Show T where
 show (App x y) = show x ++ show y
 show (Abst (Var x) y) =  "(\955" ++ (x: []) ++ ". " ++ show y ++ ")"
 show (Abst (Varj x) y) = "(\955" ++ ". " ++ show y ++ ")"
 show (Var x) = x : [' ']
 show (Varj x) = show x ++ " "
 show x = show x

dbr :: T -> T
dbr (App a b) = App (dbr a) (dbr b)
dbr (Abst (Var x) bod) = Abst (Varj 1) (dbr (subDB bod (Var x) 1) )
dbr x = x


--in T1 replace instances of T2
subDB :: T -> T -> Int -> T
subDB (Var x) (Var y) i = if (Var x)==(Var y) then (Varj i) else (Var x)
subDB (Abst (Var a) x) (Var y) i = Abst (Var a) (subDB x (Var y) (i+1))
subDB (App a b) (Var y) i = App (subDB a (Var y) i) (subDB b (Var y) i)
subDB x y i = if x==y then (Varj i) else x

testTerm =Abst (Var 'z') (App (Abst (Var 'y') (App (Var 'y') (Abst (Var 'x') (Var 'x')))) (Abst (Var 'w') (App (Var 'z') (Var 'w'))))
