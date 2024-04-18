--Code for converting math expression in string form into Postfix notation

splitter :: String -> String -> [String] -> [String]
splitter x [] outs = x : outs
splitter token (next:ins) (last:outs)
 | token == "" = splitter [next] (ins) (last:outs)
 | isCNum next && isSNum token = splitter (token ++ [next]) (ins) (last:outs)
 | isCNum next && isOp token = splitter [next] (ins) (token:last:outs)
 | next == '*' && token == "*" = splitter "**" (ins) (last:outs)
 | otherwise = splitter [next] (ins) (token:last:outs)

split :: String -> [String]
split n = merge ( filter (\w -> w /= " ") (reverse ( init (splitter "" n [""]))))

numChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']

containsChar :: Char -> [Char] -> Bool
containsChar w s = length [c | c <- s, w==c] >= 1

positions :: Eq a => a -> [a] -> [Int]
positions a as = find a (zip as [x | x <- [0..(length as)]])

isCNum :: Char -> Bool
isCNum c = containsChar c numChars

isSNum :: String -> Bool
isSNum s = (length s == length (filter (\c -> containsChar c numChars) s))

isOp :: String -> Bool
isOp n = contains n oppList
{-
split :: String -> [String]
split exp = merge [v | w <- words exp, v <- splitParens w]

splitParens :: String -> [String]
splitParens word 
 | length word == 0 = []
 | head word == '(' = ["("] ++ splitParens (tail word)
 | last word == ')' = splitParens (init word) ++ [")"]
 | otherwise = [word]

--split kinda splits math expressions in string form into list of strings that act as 'tokens'
-}
contains :: String -> [String] -> Bool
contains w s = length [c | c <- s, w==c] >= 1

oppList = ["+", "-", "*", "/", "%", "**", "(", ")"]
oppPrecedence = [2, 2, 3, 3, 3, 4]

find :: Eq a => a -> [(a, b)] -> [b]
find a pairs = [b | (a',b) <- pairs, a == a']

getPrecedence :: String -> Int
getPrecedence w
 | length precs >= 1 = head (precs)
 | otherwise = 0
 where precs = find w (zip oppList oppPrecedence)

-- need to change way to form negative numbers
mergeNeg:: String -> [String] -> [String] -> [String]
mergeNeg x [] outs = x : outs
mergeNeg cur (next:ins) (last:outs)
 | cur == "" = mergeNeg next (ins) (last:outs)
 | next == "" = (cur:outs) --Should eventually result in operator mistake
 | cur /= "-" || last == ")" = mergeNeg next (ins) (cur:last:outs)
 | last == "" && contains next oppList == False = mergeNeg (cur ++ next) (ins) (last:outs)
 | contains last oppList == True && contains next oppList == False = mergeNeg (cur++next) (ins) (last:outs)
 | otherwise = mergeNeg next (ins) (cur:last:outs)

merge :: [String] -> [String]
merge ns = reverse (init (mergeNeg "" ns [""]))
{-
Cases for converting infix:
new operator has lower precedence than current at top of stack -> add to top of stack
new operator has higher precedence than current at top of stack -> keep in token, add op at top to output
new operator has equal precedence of current and is left associative -> same as above
new operator has equal precedence but is right associative -> add to top of stack
new operator is ( -> add to top of stack
new operator is ) -> add all ops until ( to stack 

need function to handle parenthesis
need function to check if to add to top or add op to output
-}

--works now i think
--not fully done, need to work on getting order of operators selected
convertInfix :: [String] -> [String] -> [String]
convertInfix [] operators = operators
convertInfix tokens [] = []
convertInfix (token:tokens) (op : operators)
 | contains token oppList == False = token : convertInfix tokens (op:operators)
 | op == "s" = convertInfix (tokens) (token : op : operators)
 | token == ")" = handleParens (tokens) (op : operators)
 | token == "(" = convertInfix (tokens) (token: op : operators)
 | getPrecedence token > getPrecedence op || token == "**" = convertInfix (tokens) (token: op : operators)
 | getPrecedence token <= getPrecedence op = op : convertInfix (token : tokens) (operators)
 | otherwise = []

handleParens :: [String] -> [String] -> [String]
handleParens (tokens) (op:operators)
 | op == "(" = convertInfix (tokens) (operators)
 | op == "s" = convertInfix (tokens) (op:operators)
 | otherwise = op : handleParens (tokens) (operators)

convert :: [String] -> [String]
convert tokens = convertInfix tokens ["s"]

----------------------------------------------------------------------------
--Evaluation of Postfix of code

evaluate :: [String] -> [String] -> [String]
evaluate [] stack = stack
evaluate tokens [] = []
evaluate (token:tokens) (stack)
 | token == "s" = stack
 | token == "(" || token == ")" = ["ERROR: Unmatched Parentheses"]
 | contains token oppList == True && length stack <= 2 = ["ERROR: Either Missing Operands or Invalid Operator Sequence"]
 | contains token oppList == True && head rtn_val /= 'E' = evaluate (tokens) (rtn_val: (drop 2 stack))
 | contains token oppList == True && head rtn_val == 'E' = [rtn_val] 
 | isDouble token == False = ["ERROR: Invalid Operand or Operator"]
 | otherwise = evaluate (tokens) (token:stack)
 where 
  rtn_val 
   | contains token oppList == True = evaluateOp token (stack!!0) (stack!!1)
   | otherwise = "_"
{-
Error Handling:
if unmatched parenthese: will encounter ( in token
if operator w/o operands: will encounter op with length of stack = 2 (1 is taken by bottom)
if missing operator: will end up with more than 2 elements in return list (1 is taken by bottom)
if invalid character: will encounter as a token
if mismatched parentheses: will encounter ( or ) in token
if invalid operation: handle in evaluateOP, check if return from evaluate starts with E for error
if invalid operator sequence: will encounter op with length of stack = 2
if missing operand: will encounter op with length of stack = 2

So, 
Check if performing op with length of stack <= 2 -> return error that may be operation without operandsm, invalid operator sequence, or missing operand
Check for ( or ) in token -> return error of mismatched parentheses
Check if token isn't a double if it isn't an operator -> return error of invalid character
Check at very end if has more than 2 elements in return, if so return error (probably in parse function)
Check if output of evaluateOP indicates error, and return error as invalid operation
-}

doubleChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', '-']

isDouble :: String -> Bool
isDouble s = (length s == length (filter (\c -> containsChar c doubleChars) s)) && length (positions '.' s) <= 1 && length (positions '-' s) <= 1

evaluateOp :: String -> String -> String -> String
evaluateOp op val1 val2
 | op == "+" = show (v2 + v1)
 | op == "-" = show (v2 - v1)
 | op == "*" = show (v2 * v1)
 | op == "/" && v1 /= 0 = show (v2 / v1)
 | op == "**" = show (v2 ** v1)
 | op == "%" = show (doubleMod (v2) (v1))
 | otherwise = "ERROR: Invalid Operation"
 where 
  v1 = read val1 :: Double
  v2 = read val2 :: Double

--NEED to fix
doubleMod :: Double -> Double -> Double
doubleMod x y = y * ( (x/y) - fromIntegral( floor (x/y)))

--final method bringing these all together
parse :: String -> String
parse str
 | length out <= 2 = head out --read (head (evaluate (convert (split str)) ["bottom of stack"])) :: Double
 | otherwise = "ERROR: Missing Operator"
 where 
  out = (evaluate (convert (split str)) ["bottom of stack"])

{-
TODO:
1) organize functions in readable way
 - split into three sections: helper functions, conversion to PostFix, and evaluation of PostFix
2) add comments
3) test rigorously
-}