type Arg = Char
type Body = [Char]


data T = V Char |
         L Arg T |
         App T T
{--
eval :: T -> T
eval (App (L x t) (V y)) = eval 
   --}  

replace ::(Eq a)=> a -> a -> [a] -> [a]
replace _ _ [] = []
replace x y xs = map (\v-> if v==x then y else v) xs
