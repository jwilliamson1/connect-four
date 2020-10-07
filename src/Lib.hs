module Lib
    ( start
    ) where
import Control.Monad
import Data.List
import Text.Read(readMaybe)
data Player = Player1 Char | Player2 Char | Winner Char deriving (Show, Eq)
data PlayState = Play | End
type Space = Maybe Player
type Board = [[Space]] -- a list of columns

start :: IO ()
start = playLoop Play

playLoop :: PlayState -> IO ()
playLoop End = putStrLn "Thanks for playing!"
playLoop Play = do
    let p1 = Player1 'x'
    let p2 = Player2 'o'
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
            putStrLn $ show t1 ++ " enter column 1-7 to which to add."
            move <- safeReadInt :: IO Int
            b'<- retryingAddToken b (move - 1) t1
            let maybeWin = boardHasWin t1 b'
            case maybeWin of 
                (Just (_, positions)) -> do
                    putStrLn $ show t1 ++ " wins!"
                    printBoard b'
                    printBoard $ generateWinningBoard b' positions
                    putStrLn $ "Winning positions " ++ (show $ reverse positions)
                Nothing -> do printBoard b'
                              playGame t2 t1 b' 
               
safeReadInt :: IO Int
safeReadInt = do 
    str <- getLine
    let attempt = readMaybe str :: Maybe Int
    case attempt of
        (Just r) -> return r
        (_) -> do
            putStrLn "Invalid input, must be between 1-7."
            safeReadInt

retryingAddToken :: Board -> Int -> Player -> IO Board
retryingAddToken b c p = let attempt = addToken b c p
                         in case attempt of
                             (Left msg) -> do
                                 putStrLn msg
                                 move <- safeReadInt
                                 retryingAddToken b (move -1) p
                             (Right b) -> return b

replace :: [a] -> Int -> a -> [a]
replace xs n x = replaceWith xs n (\_ -> x)

replaceWith :: [a] -> Int -> (a -> a) -> [a]
replaceWith [] _ _ = []
replaceWith (l:ls) 0 f = (f l):ls
replaceWith (l:ls) i f = l : replaceWith ls (i-1) f

generateWinningBoard :: Board -> [(Int,Int)] -> Board
generateWinningBoard b xs =  foldl' bpos b xs
                             where bpos b (r, c) = replaceWith b (c - 1) (\ls -> replace ls (r - 1) $ Just winner)

winner = Winner 'W'


addToken :: Board -> Int -> Player -> Either [Char] Board
addToken board col player =
    if col < 0 || col > 6 then Left $ show player ++ "column must be between 1 and 7." else
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

detectFilled :: Board -> Bool
detectFilled b = all columnFull b
    where columnFull col = all isPlayer col
          isPlayer sp = case sp of
              Nothing -> False
              _ -> True

boardHasWin :: Player -> Board -> Maybe (Int, [(Int, Int)])
boardHasWin p b = let colWin = columnsHaveWin p b
                      rowWin = rowsHaveWin p b
                      diagonalLeftDownWin = diagonalLeftDownHasWin p b
                      diagonalRightUpWin = diagonalRightUpHasWin p b
                      wins = [colWin, rowWin, diagonalLeftDownWin, diagonalRightUpWin]
                      in msum wins
rowsHaveWin p b = let maybeWin = columnsHaveWin p $ transpose b
                    in fmap reversePositions maybeWin
                    where swap (r,c) = (c, r) 
                          reversePositions :: (Int, [(Int, Int)]) -> (Int, [(Int, Int)])                          
                          reversePositions (run, f) = (run, fmap swap f)
                            
diagonalLeftDownHasWin p b = let resWithBackwardsPos = diagonalRightUpHasWin p $ reverse b
                             in fmap (\(run, positions) -> (run, (reverse . fmap rotatePos) positions)) resWithBackwardsPos
                                where rotatePos (r, c) = (r, 7 - c + 1)

diagonalRightUpHasWin :: Player -> Board -> Maybe (Int, [(Int, Int)])
diagonalRightUpHasWin p b = slideRight 1 4 b
                          where slideRight s e cols = if s == (e + 1) then Nothing
                                else let downRes = slideDown 1 3 p cols s
                                        in case downRes of
                                            (Just a) -> Just a
                                            _ -> slideRight (s + 1) e (drop 1 cols) 

slideDown :: Int -> Int -> Player -> [[Space]] -> Int -> Maybe (Int, [(Int, Int)])
slideDown sr er p cols' curCol = if sr == (er + 1) then Nothing
else let diagRes = checkDiagonal4 sr curCol p cols'
        in case diagRes of 
            (Just a) -> Just a
            _ -> slideDown (sr + 1) er p (fmap tail cols') curCol 

checkDiagonal4 :: Int -> Int -> Player -> [[Space]] -> Maybe (Int, [(Int, Int)])
checkDiagonal4 r c p b = let playersMatch a b c' d = a == p && b == p && c' == p && d == p
                             c1 = b !! 0 !! 0
                             c2 = b !! 1 !! 1
                             c3 = b !! 2 !! 2
                             c4 = b !! 3 !! 3
                             in let res = playersMatch <$> c1 <*> c2 <*> c3 <*> c4
                                in res >>= (\match -> if match then Just (4, [(r+3, c+3), (r+2, c+2),  (r+1, c+1), (r,c) ]) else Nothing)


columnsHaveWin :: Player -> Board -> Maybe (Int, [(Int, Int)])
columnsHaveWin p b = let checkForPlayer = lineHasWin p
                    in let maybeWins = fmap checkForPlayer $ zip b [1..]
                           winningLines = filter hasWin maybeWins
                           in if length winningLines > 0 then head winningLines else Nothing 
                        where 
                            hasWin :: Maybe (Int, [(Int, Int)]) -> Bool
                            hasWin m = case m of
                                Just _ -> True
                                Nothing -> False

lineHasWin :: Player -> ([Space], Int) -> Maybe (Int, [(Int, Int)])
lineHasWin pl (sps, col) =
  let res = detectWin pl col (zip sps [1 ..])
   in if fst res > 3 then Just res else Nothing

detectWin :: Player -> Int -> [(Space, Int)] -> (Int, [(Int, Int)])
detectWin pl const sps = foldl' accumRun (0, []) sps
  where
    reset = (0, [])
    accumRun :: (Int, [(Int, Int)]) -> (Space, Int) -> (Int, [(Int, Int)])
    accumRun (count, run) sp =
      let alreadyWon = (length run) >= 4
       in case sp of
            (Nothing, _) -> if alreadyWon then (count, run) else reset
            (Just aplayer, pos) ->
              if pl == aplayer
                then (count + 1, (pos, const) : run)
                else if alreadyWon then (count, run) else reset

fillBoard :: (Functor f1, Functor f2) => f1 (f2 a1) -> a2 -> f1 (f2 (Maybe a2))
fillBoard b p = fmap (\col -> fmap (\space -> Just p) col) b

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