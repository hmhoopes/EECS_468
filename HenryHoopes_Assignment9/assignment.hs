{-
Program Name: EECS 468 Assignment 9 Nim Game
Description: This contains all functions necessary to implement the game of nim using haskell
Inputs: User input through IO() functions provided by haskell prelude
Outputs Output displayed using IO() functions provided by haskell prelude
Author: Henry Michael Hoopes
Creation Date: 4/22/2024
-}

--Defines board, alist of ints, as a datatype
type Board = [Int]

--Defines function that returns board
initial :: Board
--initial board contains list [5,4,3,2,1]
initial = [5,4,3,2,1]

--Defines function that, given two integers, displays line representing row on board
displayRow :: Int -> Int -> IO()
--when given row r and amount of stars i, do the following
displayRow r i = do
    --defines string line using list comprehension, where every other character from 1 to 2i is a *, and every other character is a space
    let line = [ch | n <- [1..2*i], let ch | n `mod` 2 == 1 = '*' | otherwise = ' '] 
    --uses putStr IO function to display the row number followed by stars
    putStr((show r) ++ ": " ++ line ++ "\n")

--Defines function that, given a board, displays all row information for that board
display :: Board -> IO()
--when given board, do the following
display board = do 
    --Display line of 80 '-' to mark beginning of round
    putStr( (take 80 (repeat '-')) ++ "\n")
    --Display first row with value of first index of board
    displayRow 1 (board!!0)
    --Display second row with value of second index of board
    displayRow 2 (board!!1)
    --Display third row with value of third index of board
    displayRow 3 (board!!2)
    --Display fourth row with value of fourth index of board
    displayRow 4 (board!!3)
    --Display fifth row with value of fifth index of board
    displayRow 5 (board!!4)

--Defines function that returns boolean representing if board represents finished game or not
isFinished :: Board -> Bool
--when given board, return true (game over) if sum of ints in board is 0
isFinished board = sum board == 0

--Defines list of strings that are valid numbers for user to enter in game
numChars = ["1","2","3","4","5"]

--defines contains method to return true if given list contains element s
contains :: Eq a => a -> [a] -> Bool
--returns true if any element in list c is equal to w
contains w s = length [c | c <- s, w==c] >= 1

--defines function that, given board and player turn, interacts with user to obtain their move that round
getMove :: Board -> Int -> IO [String]
--when given board and player turn, do the following
getMove board n = do
    --display the player turn and request player to enter row number
    putStr ("\nPlayer " ++ (show (((n+1) `mod` 2)+1)) ++ "\nEnter a row number: ")
    --store user input value in row
    row <- getLine
    --display request to player to enter number of stars to remove
    putStr ("Stars to remove: ")
    --store user input value in stars
    strs <- getLine
    --if either user input isn't value in "1".."5", it is invalid so return error indicating so
    if ( not (contains row numChars) || not (contains strs numChars) ) then
     return ["ERROR: Invalid Move"]
    --if the number of stars to remove is larger than number of stars stored in that row, it is an invalid move
    --so return error indicating so
    else if ( (read strs :: Int) > board!!((read row :: Int) - 1) ) then
     return ["ERROR: Invalid Move"]
    --if the move is valid, return string representation of moves
    else
     return [row, strs]

--defines function that, given board and two integers, makes that move on the board and returns result
playMove :: Board -> Int -> Int -> Board
--when given board, row number, and strs to remove, use list comprehension to store either boards value, or boards value after removing stars
--if row number is that index.
playMove board row strs = [val | n <- [1..5], let val | n==row = board!!(n-1) - strs | otherwise = board!!(n-1)]

--odd number rounds mean player 1's turn, even number mean play 2's turn
--defines function that, given board and player round, plays game of nim using IO
play :: Board -> Int -> IO()
--when given a board and a round turn 
play board n = do
    --display board
    display board
    --if the board represents finished game, do the following
    if (isFinished board) then
     --display the other player as winner
     putStr("Player " ++ (show (((n) `mod` 2)+1)) ++ " wins!\n")
    --if game isn't finished, do the following
    else
     do
        --get move player wants
        move <- getMove board n
        --if the move is invalid, then do the following
        if (head (head move) == 'E') then
         do
            --display move as invalid
            putStr((head move) ++ "\n")
            --play this round again so player can enter correct move
            play board n
        --if move is valid, then do the following
        else
         do
            --the following two lines convert values stored in move to integer values
            let row = read (move!!0) :: Int
            let strs = read (move!!1) :: Int
            --play next round with the result of playing move on current board
            play (playMove board row strs) (n+1)

--defines function nim that interacts with the user using io
nim :: IO()
--nim always calls play function with initial board an starting round = 1
nim = play initial 1