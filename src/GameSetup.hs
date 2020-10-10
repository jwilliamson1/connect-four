module GameSetup
    (createPlayer1,
     createPlayer2,
     createWinner,
     symb,
     createBoard,
     Space,
     Board,
     Player)
     where  

data Player = Player1 | Player2 | Winner deriving (Show, Eq)
type Space = Maybe Player
type Board = [[Space]] -- a list of columns

createPlayer1 :: Player
createPlayer1 = Player1
createPlayer2 :: Player
createPlayer2 = Player2
createWinner :: Player
createWinner = Winner

symb pl = case pl of 
            Player1 -> 'x'
            Player2 -> 'o'
            Winner -> 'W'


createBoard :: Board
createBoard = take 7 $ cycle [makeCol]
    where makeCol = take 6 $ cycle [Nothing]

