module DeBruijnSimpleLambdaTyped2 (run, update, removee,emptyContext,emptyVariables, strip, tru,fls,and', twoTrus) where
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
data Type = BOOL 
          | Other 
          | Type `ToType` Type 
          deriving (Show, Eq)

--Variable as fst, bound term as snd
data BoundThing = Bterm T
                | Btype Type 
                deriving (Show, Eq)

type Binding = (T,BoundThing)
type Variables = [Binding]
type Context = [Binding]

emptyContext = [] :: Context --handy notation
emptyVariables = [] :: Variables --footy. jk handy notation

update :: Binding -> [Binding] -> Either String [Binding]
update (x, y) lst | isVariable x = case y of
                                      (Bterm term) -> if x == term then
                                                        Left ("Error: Improper Binding. Direct Circular Referencing: " ++ show x)
                                                      else 
                                                        Right ((x,y): removee x lst)
                                      (Btype typ) -> Right ((x,y): removee x lst)             
                  | otherwise = Left ("Error: Improper Binding. Variable expected as fst. Instead found: " ++ show x)


increaseVars :: [Binding] -> [Binding]
increaseVars  = fmap (\(x,y)-> case x of
                                   (Var v) -> (Var (v+1),y)
                                   _ -> (x,y)) 

--gets what a variable is bound to. Will have a left value if not found.
gett :: T -> [Binding] -> Either String BoundThing
gett x lst | isVariable x = performLookup x lst
           | otherwise = Left ("Error: Non-variable value used as variable in getBinding. Namely: " ++ show x )

removee :: T -> [Binding]->[Binding]
removee k = filter (\x -> fst x /= k)
 
                                                                  
--a helper function for gett.
performLookup :: T -> [Binding]-> Either String BoundThing
performLookup k lst = case lookup k lst of
                     (Just x) -> Right x
                     Nothing -> Left ("Uhoh, Variable '" ++ show k ++ "' not found in context")

--does unboxing, gives empty  if malformed/left value
strip :: Either String [Binding] -> [Binding]
strip (Right c) = c
strip  _ = [] :: [Binding]




--checks if the term is a variable term or not
isVariable :: T -> Bool
isVariable (Varc _ ) = True
isVariable (Var _) = True
isVariable _ = False


--performs a type check and then evaluates if the type checks out
run :: Variables -> T -> Either String T
run vars term = case getType vars emptyContext term of
         (Right _) -> Right (eval vars term)
         (Left l) -> Left l

--determines the type of the term for a given context. 
getType :: Variables ->Context-> T-> Either String Type
getType _ _ TRUE = Right BOOL
getType _ _ FALSE = Right BOOL
getType vars context (IF x y z) = case getType vars context x of
                      (Right BOOL) -> ans 
                                      where
                                       t1 = getType vars context y
                                       t2 = getType vars context z
                                       ans = if t1 == t2 then 
                                               t1 
                                             else 
                                               Left "Error: Mismatched return types in IF expression"
                      (Right r) -> Left ("Error: BOOL expected in IF expression. Instead found: " ++ show r)
                      (Left l) -> Left ("Error: Did not resolve to a known type in IF expression conditional. Cause: " ++ show l)
{-in the case of an abstraction, we 'return' the type of the body of the abstraction. However, note that first we 
   increase all the variables in our context by 1 (since we have entered an abstraction). Then we update the context to 
   include a Var 0 that has the type the abstraction says it should have. Using this updated context, we check the type of the body.
 -}
getType vars context (Abst tp bod) = getType vars (strip( update (Var 0,Btype tp) (increaseVars context))) bod
{-if what we are substituting has the same type as the abstraction, we simply 'return' that type. Otherwise, we give a 
meaningful error message.-}
getType vars context (App (Abst tp bod) v) | abstType == vType = vType
                                           | otherwise = case (abstType, vType) of
                                                            --hindsight says a monad would have been nice here.
                                                            (Right ra,Right rv) -> Left ("Error: Substitution of wrong type: " ++ 
                                                                               show ra ++ " -and- " ++ show rv ++ " caused issue.")
                                                            (Right ra,Left lv) -> Left ("Error: Substitution variable did not" ++ 
                                                                                           "resolve to type: " ++ lv)
                                                            (Left la,Right rv) -> Left ("Error: Substitution failure. Abstraction"
                                                                                       ++ "did not resolve to type: " ++ la)
                                                            (Left la,Left lv) -> Left ("Error: Substitution failure. Abstraction"
                                                                                          ++ "did not resolve to type: " ++ la ++ 
                                                                                          "Error: Substitution variable did not" ++ 
                                                                                           "resolve to type: " ++ lv)
                                             
                                            where vType = getType vars context v
                                                  abstType = getType vars context (Abst tp bod)
--if the first part isn't an abstraction, no substitution. Therefore we are only interested in the type of the second part. 
getType vars context (App _ b) = getType vars context b
getType vars context x = case x of
                            (Var v) -> case gett (Var v) context of
                                         (Right (Btype t)) ->  Right t
                                         (Left l) -> Left l
                            (Varc v) -> case gett (Varc v) vars of
                                         (Right (Bterm r)) -> getType vars context r
                                         (Left l) -> Left l


--evaluates a term. NOTE: Assumed types check out when called.
eval :: Variables -> T -> T
eval vars (App a b)  | isReducible a = eval vars (App (eval vars a) b) --E-APP1 Vars shouldn't change so I think okay for now
                     | isReducible b = eval vars (App a (eval vars b)) --E-APP2
                     | otherwise     = case a of
                                         --previously I increased the free variables in b in this step. However, I don't think this is
                                         --correct since the substitution 'kills' the abstraction.
                                         --(Abst _ x) -> eval vars (sub (a, (increaseFrees b))) --E-APPABS
                                        (Abst _ x) -> eval vars (sub (a, b)) --E-APPABS 
                                        (Var x) -> App (Var x) (eval vars b)
                                        _ -> App (eval vars a) (eval vars b)
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
sub (Abst _ x, y) = subHelper (x,y) 0
sub (x,y) = subHelper (x,y) 0 --should never get here because sub is only called on abstractions.

--the integer indicates the 'level' of abstraction at which we are.
subHelper :: (T, T) -> Int -> T
subHelper (Abst tp x,y) i   = Abst tp (subHelper (x,y) (i+1))
subHelper (App a b, y) i = App (subHelper (a,y) i) (subHelper (b,y) i)
subHelper (Var x,y) i    = if x == i then y else Var x 

--a (perhaps now) unneeded function
increaseFrees :: T -> T
increaseFrees (App a b) = App (increaseFrees a) (increaseFrees b)
increaseFrees (Abst tp a)  = Abst tp (ifHelper a 0)
increaseFrees x = x

ifHelper :: T -> Int -> T
ifHelper (Abst tp x) i  = Abst tp (ifHelper x (i +1))
ifHelper (App a b) i = App  (ifHelper a i)  (ifHelper a i)
ifHelper (Var x) i   = Var (if x > i then x + 1 else x)


--testing helpers
tru = Abst t (Abst t (Var 1))
fls = Abst t (Abst t (Var 0))
and' = Abst t (Abst t (App (App (Var 1) (Var 0)) fls))

vars = strip (update (Varc 'y', Bterm (IF TRUE FALSE TRUE)) (strip (update (Varc 'x', Bterm TRUE) [])))
twoTrus = App (App and' tru) tru


t = BOOL
tf = BOOL `ToType` BOOL


{-- testing
prompt> run emptyContext (App tru (Varc 'x'))
Left "Substitution of wrong type caused by: Uhoh, Variable not found in context"

prompt> let c = striptoContext (addBinding [] ((Varc 'x'), (Var 88)))
prompt> run c (App tru (Varc 'x'))
Left "Substitution of wrong type caused by: Uhoh, Variable 'Var 88' not found in context"

--}
