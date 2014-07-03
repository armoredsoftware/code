module DeBruijnSimpleLambdaTyped2 where
import Data.Maybe
import Data.Map (delete)
data T =   Var Int
         | Varc Char
         | Abst Type T
         | App T T 
         | TRUE
         | FALSE
         | IF T T T
         deriving (Eq, Show)


--the Other type is for correctness checking
data Type = BOOL | Other | Type `ToType` Type deriving (Show, Eq)

--Variable as fst, bound term as snd
type Binding = (T,T)
type Context = [Binding]
emptyContext = ([] :: Context) --handy


--adds a variable binding to the given context.
addBinding :: Context -> (T,T) -> Either String Context
addBinding c (x,y) | isVariable x = if x == y then 
                                     Left ("Circular Variable Assignment not allowed! Cause: " ++ (show x) )
                                    else Right ((x,y):c)
                   | otherwise = Left ("Error: Improper Binding. Variable expected as fst. Instead found: " ++ (show x))

--performs the lookup for what the value of a variable is. 
getBinding :: Context -> T -> Either String T
getBinding c v | isVariable v = performLookup c v
               | otherwise = Left ("Error: Non-variable value used as variable in getBinding. Namely: " ++ (show v) )

--removes the FIRST occurance of the given key
removeBinding :: Context -> T -> Context
removeBinding c k = removeBindingHelper c k emptyContext

removeBindingHelper :: Context -> T -> Context -> Context
removeBindingHelper [] _ new = new
removeBindingHelper (o:ld) key new = if (fst o) == key then 
                                   new ++ ld 
                                  else
                                   removeBindingHelper ld key ( new ++ [o])
                                 
                                   

--a helper function for getBinding.
performLookup :: Context -> T -> Either String T
performLookup c v = case (lookup v c) of
                     (Just x) -> Right x
                     Nothing -> Left ("Uhoh, Variable '" ++ (show v) ++ "' not found in context")

--does unboxing, gives empty context if malformed/left value
striptoContext :: Either String Context -> Context
striptoContext (Right c) = c
striptoContext  _ = emptyContext



--checks if the term is a variable term or not
isVariable :: T -> Bool
isVariable (Varc _ ) = True
--isVariable (Var _) = True
isVariable _ = False


--performs a type check and then evaluates if the type checks out
run :: Context -> T -> Either String T
run c x = case (getType c x 0) of
         (Right _) -> Right (eval c x)
         (Left l) -> Left l

--determines the type of the term for a given context. 
getType :: Context -> T ->Int-> Either String Type
getType _ TRUE _ = Right BOOL
getType _ FALSE _ = Right BOOL
getType c (IF x y z) i = case (getType c x i) of
                      (Right BOOL) -> ans 
                                      where
                                       t1 = getType c y i
                                       t2 = getType c z i
                                       ans = if t1 ==t2 then t1 else Left "Error: Mismatched return values in IF expression"
                      (Right _) -> Left "Error: BOOL expected in IF expression"
                      (Left _) -> Left "Error: Did not resolve to a known type in IF expression conditional"
getType c (Abst tp bod) i = case tp of
                       (t1 `ToType` t2) -> Right tp
                       (x) -> Left "Malformed function type"
getType c (App (Abst tt _) v) i = case tt of --check the type of the function to be correct, like above case
                                 (t1 `ToType` t2) -> if x == (Right t1) then 
                                                      Right t2 
                                                     else  --case statement for more meaningful errors
                                                      case x of
                                                       (Right r) -> 
                                                            Left ("Error: Substitution of wrong type" ++ (show r) ++ " caused issue.")
                                                       (Left l) -> Left ("Error: Substitution of wrong type caused by: " ++ l  )
                                                     where x = getType c v i
                                 _ -> Left "Error: Malformed fucntion type"
getType c (App _ b) i = getType c b i
getType c x i | isVariable x = case (getBinding c x) of
                              (Right r) -> getType c r i
                              (Left l) -> Left l
              | otherwise = Left "Programmer error: Non-exhaustive pattern matching in getType"

 
--evaluates a term. Assumed types check out when called.
eval :: Context -> T -> T
eval c (App a b)  | isReducible a = eval c (App (eval c a) b) --E-APP1    Context shouldn't be changing so I think this is okay for now
                  | isReducible b = eval c (App a (eval c b)) --E-APP2
                  | otherwise     = case a of
                                     (Abst _ x)  -> eval c (sub ( a, (increaseFrees b))) --E-APPABS
                                     (Var x)   -> App (Var x) (eval c b)
                                     (_)       -> App (eval c a) (eval c b) --should never get here
eval c (IF x y z) | x == TRUE  = eval c y
                  | x == FALSE = eval c z
                  | otherwise  = eval c (IF (eval c x) y z)
eval c x | isVariable x = case performLookup c x of
                           (Right r) -> eval c r
                           _ -> Varc 'Z' --should never get here if argument to eval is first type checked.
         | otherwise = x

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
subHelper ((Var x),y) i    = if x == i then y else (Var x) 

increaseFrees :: T -> T
increaseFrees (App a b) = App (increaseFrees a) (increaseFrees b)
increaseFrees (Abst tp a)  = Abst tp (ifHelper a 0)
increaseFrees x = x

ifHelper :: T -> Int -> T
ifHelper (Abst tp x) i  = Abst tp (ifHelper x (i +1))
ifHelper (App a b) i = App  (ifHelper a i)  (ifHelper a i)
ifHelper (Var x) i   = if x > i then Var (x+1) else (Var x)

tru = (Abst t (Abst t (Var 1)))
fls = (Abst t (Abst t (Var 0)))
and' = (Abst t (Abst t (App (App (Var 1) (Var 0)) fls)))


t = BOOL
tf = BOOL `ToType` BOOL


{-- testing
prompt> run emptyContext (App tru (Varc 'x'))
Left "Substitution of wrong type caused by: Uhoh, Variable not found in context"

prompt> let c = striptoContext (addBinding [] ((Varc 'x'), (Var 88)))
prompt> run c (App tru (Varc 'x'))
Left "Substitution of wrong type caused by: Uhoh, Variable 'Var 88' not found in context"

--}
