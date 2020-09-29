module Lib
    ( start
    ) where
import Control.Monad
import Data.List
import Text.Read(readMaybe)
data Player = Player1 Char | Player2 Char deriving (Show, Eq)
data PlayState = Play | End
type Space = Maybe Player
type Board = [[Space]] -- a list of columns

-- addChip :: Board -> Int -> a -> Boards
-- addChip board col marker = !!
start :: IO ()
start = playLoop Play

playLoop :: PlayState -> IO ()
playLoop End = putStrLn "Thanks for playing!"
playLoop Play = do
    let p1 = Player1 'x'
    let p2 = Player2 'y'
    playGame p1 p2 createBoard
    putStrLn "GAME OVER."
    putStrLn "Enter 'y' to play again or any other key to stop."
    continue <- getLine
    let newState = if head continue == 'y' then Play else End
    playLoop newState

playGame :: Player -> Player -> Board -> IO ()
playGame t1 t2 b =
                     if detectFilled b
                        then do
                            putStrLn "IT'S A TIE! NO MORE SPACES AVAILABLE!"
                            printBoard b
                        else
                            do
                                putStrLn $ show t1 ++ " enter column 0-6 to which to add."
                                move <- safeReadInt :: IO Int
                                b'<- retryingAddToken b move t1
                                let maybeWin = boardHasWin t1 b'
                                case maybeWin of 
                                    (Just (runLength, positions)) -> do
                                        putStrLn $ show t1 ++ " wins!"
                                        printBoard b'
                                    Nothing -> do printBoard b'
                                                  playGame t2 t1 b' 
               
safeReadInt :: IO Int
safeReadInt = do 
    str <- getLine
    let attempt = readMaybe str :: Maybe Int
    case attempt of
        (Just r) -> return r
        (_) -> do
            putStrLn "Invalid input, must be between 0-5."
            safeReadInt

retryingAddToken :: Board -> Int -> Player -> IO Board
retryingAddToken b c p = let attempt = addToken b c p
                         in case attempt of
                             (Left msg) -> do
                                 putStrLn msg
                                 move <- safeReadInt
                                 retryingAddToken b move p
                             (Right b) -> return b

replace :: [a] -> Int -> a -> [a]
replace [] i el = []
replace (l:ls) 0 el = el:ls
replace (l:ls) i el = l : replace ls (i-1) el

addToken :: Board -> Int -> Player -> Either [Char] Board
addToken board col player =
    if col < 0 || col > 6 then Left $ show player ++ "column must be between 0 and 6." else
    let selectedColumn = board !! col
    in if canAddToColumn selectedColumn
        then Right $ replace board col $ addToColumn selectedColumn player
        else Left $ "Cannot add to column: " ++ show col
        where canAddToColumn column = if column !! 5 == Nothing && column /= []
                then True
                else False

addToColumn :: [Space] -> Player -> [Space]
addToColumn [] _ = []
addToColumn (x:xs) marker = if x == Nothing then Just marker:xs else x : addToColumn xs marker

createBoard :: Board
createBoard = take 7 $ cycle [makeCol]
    where makeCol = take 6 $ cycle [Nothing]

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
boardHasWin :: Player -> Board -> Maybe (Int, [Int])
boardHasWin p b = let colWin = columnsHaveWin p b
                      rowWin = rowsHaveWin p b
                      diagonalLeftDownWin = diagonalLeftDownHasWin p b
                      diagonalRightUpWin = diagonalRightUpHasWin p b
                      wins = [colWin, rowWin, diagonalLeftDownWin, diagonalRightUpWin]
                      in msum wins
rowsHaveWin p b = columnsHaveWin p $ transpose b

diagonalLeftDownHasWin p b = diagonalRightUpHasWin p $ reverse b

diagonalRightUpHasWin :: Player -> Board -> Maybe (Int, [Int])
diagonalRightUpHasWin p b = slideRight 0 3 b
                          where slideRight s e cols = if s == (e + 1) then Nothing
                                else let downRes = slideDown 0 2 p cols s
                                        in case downRes of
                                            (Just a) -> Just a
                                            _ -> slideRight (s + 1) e (drop 1 cols) 

slideDown :: Int -> Int -> Player -> [[Space]] -> Int -> Maybe (Int, [Int])
slideDown sr er p cols' curCol = if sr == (er + 1) then Nothing
else let diagRes = checkDiagonal4 sr curCol p cols'
        in case diagRes of 
            (Just a) -> Just a
            _ -> slideDown (sr + 1) er p (fmap tail cols') curCol 

checkDiagonal4 :: Int -> Int -> Player -> [[Space]] -> Maybe (Int, [Int])
checkDiagonal4 r c p b = let playersMatch a b c' d = a == p && b == p && c' == p && d == p
                             c1 = b !! 0 !! 0
                             c2 = b !! 1 !! 1
                             c3 = b !! 2 !! 2
                             c4 = b !! 3 !! 3
                             in let res = playersMatch <$> c1 <*> c2 <*> c3 <*> c4
                                in res >>= (\r' -> if r' then Just (r, [c, c+1, c+2, c+3]) else Nothing)


columnsHaveWin :: Player -> Board -> Maybe (Int, [Int])
columnsHaveWin p b = let checkForPlayer = lineHasWin p
                    in let maybeWins = fmap checkForPlayer b
                           winningLines = filter hasWin maybeWins
                           in if length winningLines > 0 then head winningLines else Nothing 
                        where 
                            hasWin :: Maybe (Int, [Int]) -> Bool
                            hasWin m = case m of
                                Just _ -> True
                                Nothing -> False

lineHasWin :: Player -> [Space] -> Maybe (Int, [Int])
lineHasWin p sps =
  let res = detectWin p (zip sps [1 ..])
   in if fst res > 3 then Just res else Nothing

detectWin :: Player -> [(Space, Int)] -> (Int, [Int])
detectWin pl sps = foldl' accumRun (0, []) sps
  where
    reset = (0, [])
    accumRun :: (Int, [Int]) -> (Space, Int) -> (Int, [Int])
    accumRun (count, run) sp =
      let alreadyWon = (length run) >= 4
       in case sp of
            (Nothing, _) -> if alreadyWon then (count,run) else reset
            (Just aplayer, pos) ->
              if pl == aplayer
                then (count + 1, pos : run)
                else if alreadyWon then (count,run) else reset

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
fp1 = fillBoard b' p1
fp2 = fillBoard b' p2
res = case e3 of Left l -> putStrLn l; Right r -> printBoard r

dlBoard = [[Just p1, Nothing, Nothing, Nothing, Nothing, Nothing],
                    [Just p1, Just p1, Nothing, Nothing, Nothing, Nothing],
                    [Just p1, Nothing, Just p1, Nothing, Nothing, Nothing],
                    [Just p1, Nothing, Nothing, Just p1, Nothing, Nothing],
                    [Just p1, Nothing, Nothing, Nothing, Just p1, Nothing],
                    [Just p1, Nothing, Nothing, Nothing, Nothing, Just p1],
                    [Just p1, Nothing, Nothing, Nothing, Nothing, Nothing]]


dl2Board = [[Just p1, Nothing, Nothing, Nothing, Nothing, Nothing],
                    [Just p1, Just p2, Nothing, Nothing, Nothing, Nothing],
                    [Just p1, Nothing, Just p1, Nothing, Nothing, Nothing],
                    [Just p1, Nothing, Nothing, Just p1, Nothing, Nothing],
                    [Just p1, Nothing, Nothing, Nothing, Just p1, Nothing],
                    [Just p1, Nothing, Nothing, Nothing, Nothing, Just p1],
                    [Just p1, Nothing, Nothing, Nothing, Nothing, Nothing]]