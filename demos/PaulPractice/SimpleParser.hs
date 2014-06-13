module SimpleParser where

{-- As step 1, write evaluation function(s) for the BNF grammar shown below.

Feel free to write these functions “naively” or 
in the fix point style we saw in the fold/unfold paper.
This will be our base language that we’ll extend to demonstrate 
how to use monads to structure effects, add new features, etc.

    a ::= nat
        | a + a
        | a - a
        | a × a

    b ::= true
        | false
        | a = a
        | a ≤ a
        | not b
        | b and b
        --}
        
        
data A a= Val a | A a `Plus` A a
             | A a `Minus` A a
             | A a `Times` A a deriving Show

          
eval :: A Int -> Int 
eval (Val x) = x
eval (x `Plus` y) = (eval x) + (eval y)
eval (x `Minus` y) = (eval x) - (eval y)
eval (x `Times` y) = (eval x) * (eval y)

{-- test data:
        eval $ (Val 7) `Plus` (Val 8) `Minus` (Val 2) `Times` (Val 3)
        --}

data  B a  = TRUE | FALSE
              | A a `Eq` A a
              | A a `LtEq` A a
              | Not (B a) 
              | B a `And` B a


eval2 :: B Int -> Bool
eval2 TRUE = True
eval2 FALSE = False
eval2 (x `Eq` y) = (eval x) == (eval y)
eval2 (x `LtEq` y) = (eval x) <= (eval y)
eval2 (Not x) = not (eval2 x)
eval2 (x `And` y) = and [(eval2 x), (eval2 y)]

