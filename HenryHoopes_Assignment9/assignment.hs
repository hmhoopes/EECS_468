type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

displayRow :: Int -> Int -> IO()
displayRow r i = do
    let line = [ch | n <- [1..2*i], let ch | n `mod` 2 == 1 = '*' | otherwise = ' '] 
    putStr((show r) ++ ": " ++ line ++ "\n")


display :: Board -> IO()
display board = do 
    putStr( (take 80 (repeat '-')) ++ "\n")
    displayRow 1 (board!!0)
    displayRow 2 (board!!1)
    displayRow 3 (board!!2)
    displayRow 4 (board!!3)
    displayRow 5 (board!!4)

isFinished :: Board -> Bool
isFinished board = sum board == 0

numChars = ["1","2","3","4","5"]
--defines contains method to return true if given list contains element s
contains :: Eq a => a -> [a] -> Bool
--returns true if any element in list c is equal to w
contains w s = length [c | c <- s, w==c] >= 1


getMove :: Board -> Int -> IO [String]
getMove board n = do
    putStr ("\nPlayer " ++ (show ((n `mod` 2)+1)) ++ "\nEnter a row number: ")
    row <- getLine
    putStr ("Stars to remove: ")
    strs <- getLine
    if ( not (contains row numChars) || not (contains strs numChars) ) then
     return ["ERROR: Invalid Move"]
    else if ( (read strs :: Int) > board!!((read row :: Int) - 1) ) then
     return ["ERROR: Invalid Move"]
    else
     return [row, strs]

playMove :: Board -> Int -> Int -> Board
playMove board row strs = [val | n <- [1..5], let val | n==row = board!!(n-1) - strs | otherwise = board!!(n-1)]

--even number rounds mean player 1's turn, odd number mean play 2's turn
play :: Board -> Int -> IO()
play board n = do
    display board
    if (isFinished board) then
     putStr("Player " ++ (show (((n+1) `mod` 2)+1)) ++ " wins!\n")
    else
     do
        move <- getMove board n
        if (head (head move) == 'E') then
         do
            putStr((head move) ++ "\n")
            play board n
        else
         do
            let row = read (move!!0) :: Int
            let strs = read (move!!1) :: Int
            play (playMove board row strs) (n+1)

nim :: IO()
nim = play initial 0