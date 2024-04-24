{-
Program Name: EECS 468 Assignment 8 Expression Parse
Brief Description: This file contains all the necessary code/functions for parsing a mathematical expression and 
                    returning the result. To use, call parse function with expression in string form as argument
Inputs: Math expression in string form
Outputs: Either value in string form or error statement indicating why expression couldn't be evaluated
Author: Henry Michael Hoopes
Creation Date: 04/15/2024
-}

---------------------------------------------------------------------------------------------------
--      Helper Functions

--defines list of characters that could be used for numbers
numChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.']

--defines contains method to return true if given list contains element s
contains :: Eq a => a -> [a] -> Bool
--returns true if any element in list c is equal to w
contains w s = length [c | c <- s, w==c] >= 1

--defines find which finds second element in list of pairs that is pair of given element
find :: Eq a => a -> [(a, b)] -> [b]
--for list of pairs, return list of elements b with pair equal to a
find a pairs = [b | (a',b) <- pairs, a == a']

--defines positions to return positions of elements in array
positions :: Eq a => a -> [a] -> [Int]
--uses find to find indices of element a in list as when zipped with indices
positions a as = find a (zip as [x | x <- [0..(length as)]])

--returns if char is in list of numbers
isCNum :: Char -> Bool
isCNum c = contains c numChars

--returns if string is a number
isSNum :: String -> Bool
--returns if every char in s is a number
isSNum s = (length s == length (filter (\c -> contains c numChars) s))

--returns if string is in list of operators
isOp :: String -> Bool
isOp n = contains n oppList

--list of operators
oppList = ["+", "-", "*", "/", "%", "**", "(", ")"]

--list of operator precedence, in order of operator list (missing precedence for () b/c they have none)
oppPrecedence = [2, 2, 3, 3, 3, 4]

--returns the precedence of an operator
getPrecedence :: String -> Int
--given operator w, do the following
getPrecedence w
 --if precs has one or more elements, return first
 | length precs >= 1 = head (precs)
 --else, if no precedence, set to 0
 | otherwise = 0
 --set precs = the results of finding pair of operator w in list of operators paired with precedence
 where precs = find w (zip oppList oppPrecedence)

--defines list of characters in a string representation of a double
doubleChars = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', '-']

--defines function that takes in a string and returns if it can be read as a double
isDouble :: String -> Bool
--given s, return if every character is a double character, and if there is at most one . and at most one - in it, and if it isn't just a minus sign
isDouble s = (length s == length (filter (\c -> contains c doubleChars) s)) && length (positions '.' s) <= 1 && length (positions '-' s) <= 1 && s /= "-"

---------------------------------------------------------------------------------------------------
--Code for converting math expression in string form list of elements in string form

--Recursive funtion designed to take in a token (starts as ""), string to parse, and list of output (starts empty), and return a list of
--elements seperated
splitter :: String -> String -> [String] -> [String]
--if string is empty, return current token followed by output
splitter x [] outs = x : outs
--for the case of token, next char, and last token, do the following:
splitter token (next:ins) (last:outs)
 --if the token is "", skip it and recurse with token as the next char (converted to string)
 | token == "" = splitter [next] (ins) (last:outs)
 --if the next char is a number, and the current token is a number, recurse with next char added to current token
 | isCNum next && isSNum token = splitter (token ++ [next]) (ins) (last:outs)
 --if the next char is * and token is *, then this is meant to be single operator, so recurse with ** as token (only operator >1 char long)
 | next == '*' && token == "*" = splitter "**" (ins) (last:outs)
 --otherwise, move token to output and recurse with next char as token (converted to string)
 | otherwise = splitter [next] (ins) (token:last:outs)

--recursive function designed to take in current element (starts as ""), list of elements to parse through, and list of output (starts as [""]), 
--and return list of all negative expressions merged together
mergeNeg:: String -> [String] -> [String] -> [String]
--if remaining string is empty, return current token followed by output
mergeNeg x [] outs = x : outs
--for the case of cur, next string, and last element, do the following:
mergeNeg cur (next:ins) (last:outs)
 --if current is "", skip it and recurse with next as the next token
 | cur == "" = mergeNeg next (ins) (last:outs)
 --if current isn't a minus sign, or if last element was end parenth, move onto next element, moving cur to output list
 | cur /= "-" || last == ")" || isOp next = mergeNeg next (ins) (cur:last:outs)
 --if current is minus, next isn't an operator, and last is empty or an operator, add minus to next element
 | last == "" || isOp last = mergeNeg (cur ++ next) (ins) (last:outs)
 --otherwise, move onto next element, moving cur to output list
 | otherwise = mergeNeg next (ins) (cur:last:outs)

--this string recieves split list and merges all negative expressions together
merge :: [String] -> [String]
--for list of strings ns, merge negatives, then remove last element (""), and reverse it so that it is in correct order
merge ns = reverse (init (mergeNeg "" ns [""]))

--this function splits a string into a proper list of elements that can be handed into the Postfix converter
split :: String -> [String]
--for string n, first use splitter to split into list, then remove the last element (""), then reverse it so that it is in correct order
--then, remove all spaces from the list, and then pass it to merge to merge all negative expressions properly
split n = merge ( filter (\w -> w /= " ") (reverse ( init (splitter "" n [""]))))

--------------------------------------------------------------------------------------------------------------------------
-- Code for converting list of strings into postfix order
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

--this is a recursive function that intakes a list of strings representing remaining input and a list to store operators, and 
--returns the list in postfix form (according to shunting yard algorithm)
convertInfix :: [String] -> [String] -> [String]
--if no remaining input, return operators
convertInfix [] operators = operators
--otherwise, given a token and list of tokens, and operator and list of operators, do the following:
convertInfix (token:tokens) (op : operators)
 --if token isn't an op, add it to list of output, and recurse on to with following tokens
 | not (isOp token) = token : convertInfix tokens (op:operators)
 --if token is an op, and the front of operator list is s, representing bottom, move token to operator list and recurse
 | op == "s" = convertInfix (tokens) (token : op : operators)
 --if the token is an right parenthesis, move to function to handle parenthese
 | token == ")" = handleParens (token:tokens) (op : operators)
 --if the token is a left parenthesis, recurse with parenthis moved to operator list
 | token == "(" = convertInfix (tokens) (token: op : operators)
 --if the token has a higher precedence, or is a ** token (highest precedence and has special rules due to right associativity), 
 --add the token to the operator list and recurse
 | getPrecedence token > getPrecedence op || token == "**" = convertInfix (tokens) (token: op : operators)
 --if the token has a lower or equal precedence of the op, then add op to list of output and recurse with current token and remaining operators
 | getPrecedence token <= getPrecedence op = op : convertInfix (token : tokens) (operators)
 --if anything else happens, simply return empty list
 | otherwise = []

--recursive function designed to handle parentheses according to shunting yard
--intakes a list of tokens, list of operators, and returns list of output in postfix order 
--(not complete, but results in complete postfix when called from convertInfix)
handleParens :: [String] -> [String] -> [String]
--given tokens and operators, do the following:
handleParens (token:tokens) (op:operators)
 --if op is left parenthesis, return to convertInfix and delete current token, which is right parenthesis
 | op == "(" = convertInfix (tokens) (operators)
 --if op is s, representing end of operator list, return to convertInfix, with current token (left parenth) added to operator list (for error handling later)
 | op == "s" = convertInfix (tokens) (token:op:operators)
 --otherwise, add op to list of ouput and recurse with same tokens and reduced operator list
 | otherwise = op : handleParens (token:tokens) (operators)

--convert takes a list of strings and converts it to postfix form
convert :: [String] -> [String]
--convert calls convertInfix with tokens, and starting list of operators ["s"]
convert tokens = convertInfix tokens ["s"]

----------------------------------------------------------------------------
--Evaluation of Postfix of code

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

--defines recursive function that takes list of tokens to be processed and stack of current values and returns either error or final value
evaluate :: [String] -> [String] -> [String]
--given tokens and stack, do the following
evaluate (token:tokens) (stack)
 --if the token is s, indicating end of list of tokens, return stack
 | token == "s" = stack
 --if the token is a parenthesis, return error, as this only occurs if mismatched parentheses occured in input
 | token == "(" || token == ")" = ["ERROR: Unmatched Parentheses"]
 --if the token is a double, add it to the stack and recurse
 | isDouble token = evaluate (tokens) (token:stack)
 --if the token is an operation, but there are less than 1 numbers on the stack, return error as this indicates input w/ messed up operators
 | isOp token && length stack <= 2 = ["ERROR: Either Missing Operands or Invalid Operator Sequence"]
 --if the token is an operation and the rtrn val isn't error, add return value to stack after removing first two values and recurse
 | isOp token && head rtn_val /= 'E' = evaluate (tokens) (rtn_val: (drop 2 stack))
 --if the token is an operation and the rtn val is an error, return message indicating so
 | isOp token && head rtn_val == 'E' = [rtn_val] 
 --otherwise, if we've reached this point, the user entered a character that isn't a number or an operator, so return message indicating so
 | otherwise = ["ERROR: Invalid Operand or Operator"]
 where 
  rtn_val 
   --set return value to result of operation with top 2 stack values if the token is an operation
   | isOp token = evaluateOp token (stack!!0) (stack!!1)
   --or just a _ if not
   | otherwise = "_"

--defines function that given three strings (1st is op, last 2 are numbers) returns the result of that operation
evaluateOp :: String -> String -> String -> String
--given an operation and values, do the following
evaluateOp op val1 val2
 --if val1 or val2 aren't doubles, return invalid operation (so read doesn't call exception)
 | not (isDouble val1) || not (isDouble val2) = "ERROR: Invalid Operation"
 --if op is +, return addition as string
 | op == "+" = show (v2 + v1)
 --if op is -, return subtraction as string
 | op == "-" = show (v2 - v1)
 --if op is *, return product as string
 | op == "*" = show (v2 * v1)
 --if op is / and denominator isn't 0, return quotient as string
 | op == "/" && v1 /= 0 = show (v2 / v1)
 --if op is **, return exponential value as string
 | op == "**" = show (v2 ** v1)
 --if op is %, return modulis as string
 | op == "%" = show (doubleMod (v2) (v1))
 --else, return invalid operation
 | otherwise = "ERROR: Invalid Operation"
 where 
  --set v1 and v2 equal to double version of themselves (won't raise error b/c confirmed they are doubles)
  v1 = read val1 :: Double
  v2 = read val2 :: Double

--Function to implement double modulus in haskell, as I couldn't find one in prelude
doubleMod :: Double -> Double -> Double
--given x and y, return y multiplied times the decimal part of x/y, which is remainder
doubleMod x y = y * ( (x/y) - fromIntegral( floor (x/y)))


--defines function that uses all the functions defined before to evaluate math expression
parse :: String -> String
--given string, do the following
parse str
 --if there is more than 1 output elements in result of evaluate, return error indicating so, otherwise return output
 | length out <= 2 = head out 
 | otherwise = "ERROR: Missing Operator"
 where 
  --set output to result of evaluate when given postfix list of strings, which is made by splitting str and passing it to convert
  out = (evaluate (convert (split str)) ["bottom of stack"])
