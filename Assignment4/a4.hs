import Data.Char


data Token  = Num Double | Err String | Op String
      deriving (Show, Eq)

 -- actualcalcStack is where all the magic happens. It converts the tokens into usable numbers to do calculations with. it checks if there is no number in the stack
-- checks if there is a one number in stack
-- checks if there is two numbers in stack
-- checks if there is more than two numbers
-- checks if there is no numbers remaining in original list and the stack is full. 
-- it then checks the boolean value of every distinct category of calculations and then pops off stack and calculates according to the true value 
-- unless a number is encountered, then it just pushes the value onto the stack. 

actualcalcStack :: [Token] ->  [Token]  -> [Token] 
actualcalcStack (x:xs) []
                | isT x = actualcalcStack xs [x]
                | otherwise = [Err ": not enough args"]
actualcalcStack (x:xs) (y:[])
                | isT x = actualcalcStack xs (x:y:[])
                | checkOp x  = [Err ": not enough args"]
                | checkTrig x = actualcalcStack xs (calcTrigFunctions x (y:[]))
                | checkSimple x = actualcalcStack xs (simpleCalcs x (y:[]))
                | checkStackOp x = actualcalcStack xs (calcStackOperators x (y:[]))
                | x == (Op "clear") = actualcalcStack [] (calcStackOperators x (y:[]))
actualcalcStack (x:xs) (y:z:[])
                | isT x = actualcalcStack xs (x:y:z:[])
                | checkOp x = actualcalcStack xs (calcValue x (y:z:[]))
                | checkSpecial x= actualcalcStack xs (calcspecialValue x (y:z:[]))
                | checkTrig x = actualcalcStack xs (calcTrigFunctions x (y:z:[]))
                | checkSimple x = actualcalcStack xs (simpleCalcs x (y:z:[]))
                | checkStackOp x = actualcalcStack xs (calcStackOperators x (y:z:[]))
                | x == (Op "clear") = actualcalcStack [] (calcStackOperators x (y:z:[]))
                | otherwise = actualcalcStack xs (y:z:[])
actualcalcStack (x:xs) (y:z:ys)
                |isT x = actualcalcStack xs (x:y:z:ys)
                |checkOp x = actualcalcStack xs (calcValue x (y:z:ys))
                |checkSpecial x = actualcalcStack xs (calcspecialValue x (y:z:ys))
                | x == (Op "clear") = actualcalcStack [] (calcStackOperators x (y:z:ys))
                | checkTrig x = actualcalcStack xs (calcTrigFunctions x (y:z:ys))
                | checkSimple x = actualcalcStack xs (simpleCalcs x (y:z:ys))
                | checkStackOp x= actualcalcStack xs (calcStackOperators x (y:z:ys))
                |otherwise = actualcalcStack xs (y:z:ys)
actualcalcStack [] lst = lst


-- this is where the functopns +all and * all are computed using the foldl function to add and multiply (depending on which one) all values in stack
calcspecialValue:: Token-> [Token] -> [Token]
calcspecialValue x smk 
                    | x == (Op "+all") = Num (foldl (+) 0 (createNum smk)):[]
                    | x == (Op "*all") = Num (foldl (*) 1 (createNum smk)):[]
                    |otherwise = [ Err "dlfs"]

-- this function just sticks to trig functions: sin and cos. 
calcTrigFunctions:: Token -> [Token] ->[Token]
calcTrigFunctions x ((Num smk):s)
                    | x == (Op "sin") = (Num (sin smk)):s
                    | x == (Op "cos") = (Num (cos smk)):s
                    |otherwise = [Err "kdjslkfjs"]

-- this handles the simple or unary functions such as inc, dec, sqrt, inv
simpleCalcs :: Token -> [Token] -> [Token]
simpleCalcs x ((Num smk):s)
                     |x == (Op "inc") = (Num (smk+1)):s
                     |x == (Op "dec")  = (Num (smk -1)):s
                     |x == (Op "sqrt")  = (Num (sqrt smk)):s
                     |x == (Op "inv")  = (Num (1/smk)):s
                     |otherwise = [Err "honey no"]

-- this handles the stereotypical stack operators that you would find anywhere
calcStackOperators :: Token->[Token]->[Token]
calcStackOperators x ((Num smk):s)
                     |x == (Op "dup") = (Num smk):(Num smk):s
calcStackOperators x ((Num smk):s)
                     | x == (Op "pop") && (length s) /= 0 = s
                     | x == (Op "pop") = [Err "empty stack"]
calcStackOperators x smk 
                     | x == (Op "clear") = [Err "empty stack"]
calcStackOperators x (smk:smk2:s)
                     | x== (Op "swap") = smk2:smk:s
                     | otherwise = [Err "Swap cannot happen because of inappropriate input"]

-- this does regular +,-,*, /                
calcValue :: Token -> [Token] -> [Token]
calcValue op ((Num smk):(Num smk2):s)
         | op == (Op "+") = (Num (smk2 +smk)):s
         | op == (Op "-") = (Num (smk -smk2)):s
         | op == (Op "*") = (Num (smk *smk2)):s
         | op == (Op "/") = (Num (smk2/smk)):s
         |otherwise = [Err "something wrong with calcValue" ]



-- calcStack outputs the value of the top of the stack
calcStack :: String -> String
calcStack "" = "empty stack"
calcStack str = "Top -> " ++ (show $ actualcalcStack (createTok $ words str) [])

-- calc is the instigator function which takes input and shows the final output. 
calc :: String -> String
calc [] = "empty stack"
calc tokens1 = returnString $ head $ (actualcalcStack( createTok $ words tokens1) [])

-- this detokenizes the values so it can be read as string for the calc function. 
returnString :: Token -> String
returnString (Num x) = show x
returnString (Err x) = x
returnString (Op x) = x
     
 -- checks if were are deeling with a numbered token. 
isT:: Token -> Bool 
isT (Num x) = True
isT (Err x) = False
isT (Op x) = False 

-- checks if we are dealing with +,-,/,*
checkOp ::  Token -> Bool 
checkOp x | x == (Op "+") = True
          | x == (Op "-") = True
          | x == (Op "/") = True
          | x == (Op "*") = True
          | otherwise = False

-- checks if we are dealing with trig functions
checkTrig::Token -> Bool
checkTrig x | x == (Op "sin") = True
            | x == (Op "cos") = True
            |otherwise = False

-- checks if we are dealing with +all or *all
checkSpecial :: Token -> Bool
checkSpecial x | x == (Op "+all") || x == (Op "*all") = True
               |otherwise = False

-- checks if we are dealing with unary operators. 
checkSimple :: Token -> Bool
checkSimple x | x == (Op "inc") || x == (Op "dec") || x == (Op "inv") || x == (Op "sqrt") = True
              | otherwise = False

-- checks if we are dealing with stack operators
checkStackOp:: Token -> Bool
checkStackOp x | x == (Op "swap") || x == (Op "pop") || x == (Op "dup") = True
               | otherwise = False
               
-- used for foldl so it can work with actual numbers and not tokens. 
createNum :: [Token] -> [Double]
createNum [] = []
createNum ((Num n):ns) = n :(createNum ns)
createNum _ = []

--converts all the strings to tokens. 

createTok :: [String] -> [Token]
createTok [] = []
createTok (string:strings)
                 | (string == "+") = (Op "+"):createTok strings
                 | (string == "-") = (Op "-"):createTok strings
                 | (string == "*") = (Op "*"):createTok strings
                 | (string == "/")= (Op "/"):createTok strings
                 | (string == "inc") = (Op "inc"):createTok strings
                 | (string == "dec") = (Op "dec"):createTok strings
                 | (string == "sqrt") = (Op "sqrt"):createTok strings
                 | (string == "sin") = (Op "sin"):createTok strings
                 | (string == "cos") = (Op "cos"):createTok strings
                 | (string == "inv") = (Op "inv"):createTok strings
                 | (string == "+all") = (Op "+all"):createTok strings
                 | (string == "*all") = (Op "*all"):createTok strings
                 | (string == "dup") = (Op "dup"):createTok strings
                 | (string == "pop") = (Op "pop"):createTok strings
                 | (string == "clear") = (Op "clear"):createTok strings
                 | (string == "swap") =(Op "swap"):createTok strings
                 | (checkNum string) = (Num (read string :: Double)):createTok strings
                 | otherwise = [Err "cannot convert to Token"]
-- checks if string in createTok is a number and then converts it to a double.
checkNum :: String -> Bool
checkNum x = elem x ["0","1","2","3","4","5","6","7","8","9","."]














