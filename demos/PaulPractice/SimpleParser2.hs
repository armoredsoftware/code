--module SimpleParser where

import Prelude
import Data.List
import Data.String
import System.Environment
{--Informally, commands c are described by the following BNF grammar:
 
     c ::= SKIP
         | x ::= a
         | c ;; c
         | WHILE b DO c END
         | IFB b THEN c ELSE c FI
 
For example, here's the factorial function in Imp.
 
     Z ::= X;;
     Y ::= 1;;
     WHILE not (Z = 0) DO
       Y ::= Y × Z;;
       Z ::= Z - 1
     END

(Z `Set` (Var "x"))
 
When this command terminates, the variable Y will contain the factorial of the initial value of X.
        --}
data A 
    = Val Int
    | A `Plus` A
    | A `Minus` A
    | A `Times` A 
    | Var String deriving (Show,Read)
data  B 
    = TRUE | FALSE
    | A `Eq` A
    | A `LtEq` A
    | Not B 
    | B `And` B deriving (Show, Read)
   --works:    eval ((Val 3) `Plus` (Var "s")) [("s",4)]

      
eval :: A -> Variables -> Int 
eval (Val x) v = x
eval (Var x) v = case (lookup x v) of
                          Nothing -> 0
                          Just a -> a
eval (x `Plus` y) v = (eval x v) + (eval y v)
eval (x `Minus` y) v = (eval x v) - (eval y v)
eval (x `Times` y) v = (eval x v) * (eval y v)


type Value = Int
type Variables = [(String, Value)]



eval2 :: B -> Variables -> Bool
eval2 TRUE v = True
eval2 FALSE v = False
eval2 (x `Eq` y) v = (eval x v) == (eval y v)
eval2 (x `LtEq` y) v = (eval x v) <= (eval y v)
eval2 (Not x) v = not (eval2 x v)
eval2 (x `And` y) v = and [(eval2 x v), (eval2 y v)]


main :: IO ()
main = do
       [f] <- getArgs
       contents <- readFile f
  --     let code = testRead contents in
       let code =   ultimateEvalH (constructTree $ parseToWords contents) []  in
         print (code)

       --       xs <- getArgs
--       contents <- sequence $ fmap readFile xs   
--       mapM_ putStr (contents) 

--Step 1: Get the program words
type Code = String
parseToWords:: String -> [String]
parseToWords s = words s  

data C = Code C C
         | SKIP
         | Loop BExpression [String]
         | Cond BExpression [String] [String]
         | Assign String String 
instance Show C where
  show (Code x y) = show x ++ "\n" ++ show y
  show SKIP = "SKIP"
  show (Loop x xs) = "Loop: " ++ x ++ show xs
  show (Cond x y z) = "Cond: " ++ x ++ "If true: " ++ show y ++ "Else: " ++ show z
  show (Assign x y) = "Assign: " ++ x ++ " to " ++ y


--no error checking, your code must be perfect!
  --converts a program to a list a variables that hold the results of running the program. Variables can only be
--one character capitals that hold an int.
ultimateEvalH :: C ->Variables -> Variables
ultimateEvalH (Code x y) vars = ultimateEvalH y (ultimateEvalH x vars)
ultimateEvalH SKIP vars = vars
ultimateEvalH (Loop x xs) vars | (eval2 (read x:: B) vars) =ultimateEvalH (Loop x xs) (ultimateEvalH (constructTree xs) vars)
                               | otherwise = vars
ultimateEvalH (Cond x y z) vars | (eval2 (read x:: B) vars) = ultimateEvalH (constructTree y) vars
                                | otherwise = ultimateEvalH (constructTree z) vars
ultimateEvalH (Assign x y) vars = case (lookup x vars) of
                          Nothing -> (x, (eval (read y:: A) vars)) : vars --if not present, add it!
                          Just a ->((x, (eval (read y:: A) vars)) : (filter (\p -> ((fst p) /=x) ) vars))  --if it's present, replace it!

--Converts the words of the program into a single Command
constructTree :: [String] -> C
constructTree [] = SKIP
constructTree (x : xs) | x=="WHILE" = Code (Loop (parseB xs "") (getBody xs ("DO", "END"))) (constructTree (ignoreTillBlockEnd xs ("DO", "END")))
                       | x=="IF" = Code (Cond (parseB xs "") (getBody xs ("THEN","ELSE")) (getBody xs ("ELSE","FI"))) (constructTree (ignoreTillBlockEnd xs ("IF", "FI")))
                       | length x == 1 && ((head x) `elem` ['A' .. 'Z']) = Code (Assign x (getA xs "")) (constructTree (ignoreAssignment xs))
                       | otherwise = SKIP


--gets the BExpression as a string. In this language, BExpressions are only found preceding a "DO" or a "THEN"
type BExpression= String
parseB :: [String]->String-> BExpression
parse [] r = r
parseB (x : xs) s | x == "DO" || x == "THEN" = s
                  | otherwise = parseB xs (s ++" " ++ x)

--gets the AExpression as a string. In this language, A expressions are only found in assignments.
getA :: [String] -> String -> String
getA [] bod = bod
getA (x:xs) bod | isSuffixOf ";;" x = bod ++" "++ (takeWhile (/=';') x)
                | x =="::=" = getA xs bod
                | otherwise = getA xs (bod ++ x)

--gets the rest of the program, ignoring the first assigment traversed.
ignoreAssignment :: [String] -> [String]
ignoreAssignment [] = []
ignoreAssignment (x:xs) | isSuffixOf ";;" x = xs
                        | otherwise = ignoreAssignment xs

--ignores the code in the kind of block specified by (String, String) and returns all code after 
ignoreTillBlockEnd:: [String] -> (String,String) -> [String]
ignoreTillBockEnd [] _ = []
ignoreTillBlockEnd ls (start,stop) = ignoreTillBlockEndHelper ls (start,stop) 0

ignoreTillBlockEndHelper :: [String]-> (String, String)-> Int -> [String]
ignoreTillBlockEndHelper [] _ _ = []
ignoreTillBlockEndHelper (x : xs) (start,stop) i
                                        | x==start = ignoreTillBlockEndHelper xs (start,stop) (i+1)
                                        | x==stop && i>0 = ignoreTillBlockEndHelper xs (start,stop) (i-1)
                                        | x==stop && i==0 = xs
                                        | otherwise = ignoreTillBlockEndHelper xs (start,stop) i
                                                      
--does the compliment of ignoreTillBlockEnd. Returns the code in the specified body tag (String,String)
getBody :: [String] -> (String, String) -> [String]
getBody [] _ = []
getBody (x:xs) (start,stop)  | x ==start = getBodyHelper xs [] (start,stop) 0
                             | otherwise = getBody xs (start,stop)

getBodyHelper :: [String] -> [String]->(String, String) -> Int -> [String]
getBodyHelper [] bod _ _ = bod
getBodyHelper (x:xs) bod (start,stop) i | x==start = getBodyHelper xs (bod ++ [x]) (start,stop) (i+1)
                                        | x==stop && i>0 = getBodyHelper xs (bod ++[x]) (start,stop) (i-1)
                                        | x==stop && i==0 = bod
                                        | otherwise = getBodyHelper xs (bod ++ [x]) (start,stop) i
                                                            
