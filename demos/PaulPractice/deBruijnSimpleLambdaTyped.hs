data T =   Var Type Int
         | Abst Type T
         | App T T 
         | TRUE
         | FALSE
         | IF T T T
         deriving (Eq, Show)



data Type = BOOL | Other | Type `ToType` Type deriving (Show, Eq)


--type TypePair = (T, Type)
--type Context = [TypePair]

run :: T -> Either String T
run x = case (getType x) of
         (Right _) -> Right (eval x)
         (Left l) -> Left l

getType :: T -> Either String Type
getType TRUE = Right BOOL
getType FALSE = Right BOOL
getType (IF x y z) = case (getType x) of
                      (Right BOOL) -> ans 
                                      where
                                       t1 = getType y
                                       t2 = getType z
                                       ans = if t1 ==t2 then t1 else Left "Mismatched return values in IF expression"
                      (Right _) -> Left "BOOL expected in IF expression"
                      (Left _) -> Left "Did not resolve to a known type in IF expression conditional"
getType (Var tp _) = Right tp
getType (Abst tp _) = case tp of
                       (t1 `ToType` t2) -> Right tp
                       (x) -> Left "Malformed function type"
getType (App (Abst (t1 `ToType` t2) _) v) = if x == (Right t1) then 
                                              Right t2 
                                            else 
                                             Left "Substitution of wrong type"  
                                           where x = getType v
getType (App _ b) = getType b

eval :: T -> T
eval (App a b)  | isReducible a = eval (App (eval a) b) --E-APP1
                | isReducible b = eval (App a (eval b)) --E-APP2
                | otherwise     = case a of
                                     (Abst _ x)  -> eval (sub ( a, (increaseFrees b))) --E-APPABS
                                     (Var tp x)   -> App (Var tp x) (eval b)
                                     (_)       -> App (eval a) (eval b) --should never get here
eval (IF x y z) | x == TRUE  = eval y
                | x == FALSE = eval z
                | otherwise  = eval (IF (eval x) y z)
eval x = x

isReducible :: T -> Bool
isReducible (App Abst{} _) = True
isReducible (App a b)      = isReducible a || isReducible b
isReducible _              = False

sub:: (T,T) ->T
sub ((Abst _ x), y) = subHelper (x,y) 0
sub (x,y) = subHelper (x,y) (-1) --should never get here

subHelper :: (T, T) -> Int -> T
subHelper ((Abst tp x),y) i   = Abst tp (subHelper (x,y) (i+1))
subHelper ((App a b), y) i = App (subHelper (a,y) i) (subHelper (b,y) i)
subHelper ((Var tp x),y) i    = if x == i then y else (Var tp x) 

increaseFrees :: T -> T
increaseFrees (App a b) = App (increaseFrees a) (increaseFrees b)
increaseFrees (Abst tp a)  = Abst tp (ifHelper a 0)
increaseFrees x = x

ifHelper :: T -> Int -> T
ifHelper (Abst tp x) i  = Abst tp (ifHelper x (i +1))
ifHelper (App a b) i = App  (ifHelper a i)  (ifHelper a i)
ifHelper (Var tp x) i   = if x > i then Var tp (x+1) else (Var tp x)

tru = (Abst tf (Abst tf (Var t 1)))
fls = (Abst tf (Abst tf (Var t 0)))
and' = (Abst tf (Abst tf (App (App (Var t 1) (Var t 0)) fls)))


t = BOOL
tf = BOOL `ToType` BOOL


{-- testing
*Main> getType (IF (Var Other 5) TRUE TRUE)
Left "BOOL expected in IF expression"

*Main> getType (IF TRUE TRUE (Var Other 5) )
Left "Mismatched return values in IF expression"

*Main> getType (App tru (Var Other 6))
Left "Substitution of wrong type"
-}
