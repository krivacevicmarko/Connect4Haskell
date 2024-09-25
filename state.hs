module Module.StateMonada (GameError(..),BoardState(..),GameStateOp(..),determineNextPlayer,applyMove,applyMovesFromList,applyMoves,testMoves,initialState,prePopulatedBoard,initialPrimer,initialBig,mainn,mainBig) where
import Module.Connect4
import Module.Rose
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Monad (forM)

data GameError = ColumnFull | GameAlreadyWon | InvalidMove deriving (Show)

-- Struktura stanja table
data BoardState a = BoardState {
    gameBoard :: Board a,
    playerToMove :: Player,
    gameOver :: Bool,        
    errorMsg :: Maybe String 
} deriving (Show)

newtype GameStateOp b a = GameStateOp { runGameState :: BoardState b -> (a, BoardState b) }

instance Functor (GameStateOp b) where
    fmap f (GameStateOp rs) = GameStateOp $ \s -> let (a, newState) = rs s in (f a, newState)

instance Applicative (GameStateOp b) where
    pure x = GameStateOp $ \s -> (x, s)
    (GameStateOp rs1) <*> (GameStateOp rs2) = GameStateOp $ \s ->
        let (f, newState) = rs1 s
            (a, newState1) = rs2 newState
        in (f a, newState1)

instance Monad (GameStateOp b) where
    return = pure
    (GameStateOp h) >>= f = GameStateOp $ \s ->
        let (a, newState) = h s
            (GameStateOp g) = f a
        in g newState

determineNextPlayer :: Board Field -> Player
determineNextPlayer b =
    let (cCount, zCount) = countDiscs b
    in if cCount < zCount then Crveni
       else if zCount < cCount then Zuti
       else Crveni  -- Default na Crveni ako je broj diskova jednak

applyMove :: Int -> GameStateOp Field (Either GameError Bool)
applyMove colIdx = GameStateOp $ \s ->
    let currentBoard = gameBoard s
        player = playerToMove s
    in if gameOver s
       then (Left GameAlreadyWon, s { errorMsg = Just "Potez odigran nakon zavrsene igre" })
       else if isColumnFull colIdx currentBoard
            then (Left ColumnFull, s { errorMsg = Just "Dodavanje tokena u punu kolonu" })
            else
                let newBoard = placeDisc colIdx player currentBoard
                    hasWon = checkWin newBoard
                    newPlayer = changePlayer player
                    updatedState = s {
                        gameBoard = newBoard,
                        playerToMove = newPlayer,
                        gameOver = hasWon,
                        errorMsg = if hasWon then Just ("Igrac " ++ show player ++ " je pobedio!") else Nothing
                    }
                in if hasWon
                   then (Right hasWon, updatedState { gameOver = True })  
                   else (Right hasWon, updatedState)

applyMovesFromList :: [Int] -> GameStateOp Field (Either GameError Bool)
applyMovesFromList [] = return (Right False) 
applyMovesFromList (x:xs) = do
    result <- applyMove x
    case result of
        Left err -> return (Left err)  
        Right _  -> applyMovesFromList xs  

-- runGameState applyMoves initialPrimer
-- let (result,finalState) = runGameState applyMoves initialPrimer
-- print result
-- printBoard (gameBoard finalState)
-- runGameState applyMoves2 initialPrimer
-- let (result,finalState) = runGameState applyMoves2 initialPrimer
-- printBoard (gameBoard finalState)
-- runGameState winningMoves initialState

diagonalWinning = applyMovesFromList [1,2,2,3,3,4,3,4,4,1,4]
-- let (result,finalState) = runGameState diagonalWinning initialPrimer
-- printBoard (gameBoard finalState)

applyMoves :: GameStateOp Field (Either GameError Bool)
applyMoves = do
    applyMove 2
    applyMove 2
    applyMove 2
    applyMove 1
    applyMove 1

applyMoves2 :: GameStateOp Field (Either GameError Bool)
applyMoves2 = do
    applyMove 1
    applyMove 3
    applyMove 1
    applyMove 1
    applyMove 3

applyMove1 :: GameStateOp Field (Either GameError Bool)
applyMove1 = do applyMove 1

-- Testiranje sa listom poteza
testMoves :: GameStateOp Field (Either GameError Bool)
testMoves = applyMovesFromList [1, 2, 1, 2, 1, 2, 2, 1, 1]

winningMoves :: GameStateOp Field (Either GameError Bool)
winningMoves = applyMovesFromList [1,2,1,2,1,2,1,2]

-- Kreiranje početnog stanja sa praznom tablom
initialState :: BoardState Field
initialState = BoardState {
    gameBoard = createEmptyBoard 4 5,
    playerToMove = determineNextPlayer (createEmptyBoard 4 5),
    gameOver = False,
    errorMsg = Nothing
}

prePopulatedBoard :: Board Field
prePopulatedBoard = listToBoard [
    [C, Z, C, Z, C, Z],
    [C, C, C, C, Z, C],
    [E, Z, Z, C, Z, Z],
    [E, E, E, E, C, Z],
    [E, E, E, E, C, Z]
    ]

tablaPrimer :: Board Field
tablaPrimer = listToBoard [
    [E,E,C,Z,E],
    [E,E,C,Z,E],
    [E,E,Z,E,E],
    [E,E,E,E,E]
    ]

initialPrimer :: BoardState Field
initialPrimer = BoardState {
    gameBoard = tablaPrimer,
    playerToMove = determineNextPlayer tablaPrimer,
    gameOver = checkWin tablaPrimer,
    errorMsg = if checkWin tablaPrimer
               then Just "Igra je vec zavrsena."
               else Nothing
}

initialBig :: BoardState Field
initialBig = BoardState {
    gameBoard = prePopulatedBoard,
    playerToMove = determineNextPlayer prePopulatedBoard,
    gameOver = checkWin prePopulatedBoard,
    errorMsg = if checkWin prePopulatedBoard
               then Just "Igra je vec zavrsena."
               else Nothing
}

-- Pokretanje igre sa testMoves i praznom tablom
mainn :: IO ()
mainn = do
    let (result, finalState) = runGameState testMoves initialState
    print result
    printBoard (gameBoard finalState)
    case errorMsg finalState of
        Just msg -> putStrLn msg
        Nothing  -> return ()

-- Pokretanje igre sa testMoves i prepopulisanom tablom
mainBig :: IO ()
mainBig = do
    let (result, finalState) = runGameState testMoves initialBig
    print result
    printBoard (gameBoard finalState)
    case errorMsg finalState of
        Just msg -> putStrLn msg
        Nothing  -> return ()

