module Lib
    ( start
    ) where
import Control.Monad
import Data.List

data Player = Player1 Char | Player2 Char deriving (Show, Eq)
data PlayState = Play | End
type C4Board = [[Maybe Player]]
type Space = Maybe Player
type Board = [[Space]]

-- addChip :: Board -> Int -> a -> Boards
-- addChip board col marker = !!
start :: IO ()
start = playLoop Play

-- Play GameOver 

playLoop :: PlayState -> IO ()
playLoop prevState = do
    putStrLn "Enter 'y' to continue or any other key to stop."
    continue <- getLine
    let newState = if head continue == 'y' then Play else End
    case newState of
        End -> putStrLn "Thanks for playing!"
        Play -> playLoop Play

replace :: [a] -> Int -> a -> [a]
replace [] i el = []
replace (l:ls) 0 el = el:ls
replace (l:ls) i el = l : replace ls (i-1) el

addToken :: Board -> Int -> Player -> Either [Char] Board
addToken board col player =
    let selectedColumn = board !! col
    in if canAddToColumn selectedColumn
        then Right $ replace board col $ addToColumn selectedColumn player
        else Left $ "Cannot add to column: " ++ show col
        where canAddToColumn column = if column !! 5 == Nothing
                then True
                else False

addToColumn :: [Space] -> Player -> [Space]
addToColumn [] _ = []
addToColumn (x:xs) marker = if x == Nothing then Just marker:xs else x : addToColumn xs marker

createBoard :: Board
createBoard = take 6 $ cycle [makeRow]
    where makeRow = take 7 $ cycle [Nothing]

printBoard :: Board -> IO ()
printBoard b = mapM_ putStrLn $ fmap convertRowToString $ reverse $ transpose b

convertSpaceToString :: Space -> [Char]
convertSpaceToString s = case s of
    Nothing -> "   "
    Just p -> ' ' : symb p : " "
    where symb pl = case pl of 
            (Player1 ch1) -> ch1
            (Player2 ch2) -> ch2

convertRowToString :: [Space] -> [Char]
convertRowToString ss = unwords toStrings
    where toStrings = fmap convertSpaceToString ss 

fillBoard :: Board -> Player -> Board
fillBoard b p = fmap (\col -> fmap (\space -> Just p) col) b

detectFilled :: Board -> Bool
detectFilled b = all columnFull b
    where columnFull col = all isPlayer col
          isPlayer sp = case sp of
              Nothing -> False
              _ -> True

printEitherBoard :: Either [Char] Board -> IO ()
printEitherBoard x = case x of Left err -> putStrLn err; Right b -> printBoard b

p1 = Player1 'X'
p2 = Player2 'O'
b' = createBoard
e1 = addToken b' 1 $ p1
e2 = e1 >>= (\b -> addToken b 1 $ p2) 
e3 = do
    e1 <- addToken b' 1 $ p1
    e2 <- addToken e1 1 $ p2
    addToken e2 0 p1

res = case e3 of Left l -> putStrLn l; Right r -> printBoard r