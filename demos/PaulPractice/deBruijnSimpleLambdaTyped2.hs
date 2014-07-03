module DeBruijnSimpleLambdaTyped2 where
import Data.Maybe

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
data BoundThing = Bterm T
                | Btype Type

type Binding = (T,BoundThing)
type Variables = [Binding]
type Context = [Binding]

emptyContext = ([] :: Context) --handy notation
emptyVariables = ([] :: Variables) --footy. jk handy notation

update :: Binding -> [Binding] -> Either String [Binding]
update (x, y) lst | isVariable x = case y of
                                      (Bterm term) -> if x == term then
                                                Left ("Error: Improper Binding. Variable expected as fst. Instead found: " ++ (show x))
                                                else Right ((x,y):(removee x lst))
                                      (Btype typ) -> Right ((x,y): (removee x lst))             
                  | otherwise = Left ("Error: Improper Binding. Variable expected as fst. Instead found: " ++ (show x))


increaseVars :: [Binding] -> [Binding]
increaseVars lst = fmap (\(x,y)-> case x of
                                             (Var v) -> ((Var (v+1)),y)
                                             _ -> (x,y)) lst


gett :: T -> [Binding] -> Either String BoundThing
gett x lst | isVariable x = performLookup x lst
           | otherwise = Left ("Error: Non-variable value used as variable in getBinding. Namely: " ++ (show x) )

removee :: T -> [Binding] -> [Binding]
removee k lst = filter (\x -> fst x /= k) lst
 
                                                                  
--a helper function for gett.
performLookup :: T -> [Binding]-> Either String BoundThing
performLookup k lst = case (lookup k lst) of
                     (Just x) -> Right x
                     Nothing -> Left ("Uhoh, Variable '" ++ (show k) ++ "' not found in context")

--does unboxing, gives empty  if malformed/left value
strip :: Either String [Binding] -> [Binding]
strip (Right c) = c
strip  _ = ([] :: [Binding])




--checks if the term is a variable term or not
isVariable :: T -> Bool
isVariable (Varc _ ) = True
isVariable (Var _) = True
isVariable _ = False


--performs a type check and then evaluates if the type checks out
run :: Variables -> T -> Either String T
run vars term = case (getType vars emptyContext term) of
         (Right _) -> Right (eval vars term)
         (Left l) -> Left l

--determines the type of the term for a given context. 
getType :: Variables ->Context-> T-> Either String Type
getType _ _ TRUE = Right BOOL
getType _ _ FALSE = Right BOOL
getType vars context (IF x y z) = case (getType vars context x) of
                      (Right BOOL) -> ans 
                                      where
                                       t1 = getType vars context y
                                       t2 = getType vars context z
                                       ans = if t1 ==t2 then t1 else Left "Error: Mismatched return values in IF expression"
                      (Right _) -> Left "Error: BOOL expected in IF expression"
                      (Left _) -> Left "Error: Did not resolve to a known type in IF expression conditional"
getType vars context (Abst tp bod) = getType vars (strip( update ((Var 0),(Btype tp)) (increaseVars context))) bod
--if v has the same type as tp then we're good with type tp. otherwise, give a meaningful error message.
getType vars context (App (Abst tp bod) v) | abstType == vType = vType
                                           | otherwise = case (abstType, vType) of
                                                            --hindsight says a monad would have been nice here.
                                                            ((Right ra),(Right rv)) -> Left ("Error: Substitution of wrong type: " ++ 
                                                                               (show ra) ++ " and " ++ (show rv) ++ " caused issue.")
                                                            ((Right ra),(Left lv)) -> Left ("Error: Substitution variable did not" ++ 
                                                                                           "resolve to type: " ++ lv)
                                                            ((Left la),(Right rv)) -> Left ("Error: Substitution failure. Abstraction"
                                                                                       ++ "did not resolve to type: " ++ la)
                                                            ((Left la),(Left lv)) -> Left ("Error: Substitution failure. Abstraction"
                                                                                          ++ "did not resolve to type: " ++ la ++ 
                                                                                          "Error: Substitution variable did not" ++ 
                                                                                           "resolve to type: " ++ lv)
                                             
                                            where vType = getType vars context v
                                                  abstType = getType vars context (Abst tp bod)
--if the first part isn't an abstraction, no substitution. Therefore I am only interested in the type of the second part. 
getType vars context (App _ b) = getType vars context b
getType vars context x = case x of
                            (Var x) -> case (gett (Var x) context) of
                                         (Right (Btype t)) ->  Right t
                                         (Left l) -> Left l
                            (Varc x) -> case (gett (Varc x) vars) of
                                         (Right (Bterm r)) -> getType vars context r
                                         (Left l) -> Left l

--evaluates a term. Assumed types check out when called.
eval :: Variables -> T -> T
eval vars (App a b)  | isReducible a = eval vars (App (eval vars a) b) --E-APP1    Context shouldn't be changing so I think this is okay for now
                  | isReducible b = eval vars (App a (eval vars b)) --E-APP2
                  | otherwise     = case a of
                                     (Abst _ x)  -> eval vars (sub ( a, (increaseFrees b))) --E-APPABS
                                     (Var x)   -> App (Var x) (eval vars b)
                                     (_)       -> App (eval vars a) (eval vars b) --should never get here
eval vars (IF x y z) | x == TRUE  = eval vars y
                  | x == FALSE = eval vars z
                  | otherwise  = eval vars (IF (eval vars x) y z)
eval vars x | isVariable x = case performLookup x vars of
                           (Right (Bterm r)) -> eval vars r
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
