

module Module.Connect4 (Player(..),Row(..),Board(..),Field(..),rowToList,listToRow,boardToList,listToBoard,playerSymbol,fieldToStr,createEmptyBoard,printBoard,placeDisc,isBoardFull,gameEnded,isColumnFull,validMoves,checkWin,changePlayer,randomFirstPlayer,countDiscs,generateGameTree) where

import Data.List (transpose, tails)
import System.Random (randomRIO)
import Module.Rose

data Player = Crveni | Zuti deriving (Show, Eq)
newtype Row a = Row [a] deriving (Show, Eq)
newtype Board a = Board [Row a] deriving (Show, Eq)
data Field = C | Z | E deriving (Show, Eq, Enum)

data GameState a = GameState {
    gameBoard :: Board a,
    playerToMove :: Player
} deriving (Show)

rowToList :: Row a -> [a]
rowToList (Row fields) = fields

listToRow :: [a] -> Row a
listToRow = Row

boardToList :: Board a -> [[a]]
boardToList (Board rows) = map rowToList rows

listToBoard :: [[a]] -> Board a
listToBoard rows = Board (map Row rows)

playerSymbol :: Player -> Field
playerSymbol Crveni = C
playerSymbol Zuti = Z

fieldToStr :: Field -> String
fieldToStr x = case x of
    C -> "C"
    Z -> "Z"
    E -> " "

-- Kreiranje prazne table
createEmptyBoard :: Int -> Int -> Board Field
createEmptyBoard rows cols = listToBoard (replicate rows (replicate cols E))

-- Ispisivanje table
printBoard :: Board Field -> IO ()
printBoard board = do
    let rows = boardToList board
    mapM_ printRow (reverse rows)
  where
    printRow row = putStrLn $ "|" ++ concatMap (\cell -> " " ++ fieldToStr cell ++ " |") row

-- Postavljanje diska u kolonu
placeDisc :: Int -> Player -> Board Field -> Board Field
placeDisc colIdx player (Board rows) =
    let colIdx' = colIdx - 1
        -- Nađi prvi slobodan red odozdo
        (above, current:below) = span (\row -> (rowToList row !! colIdx') /= E) rows 
        newRow = take colIdx' (rowToList current) ++ [playerSymbol player] ++ drop (colIdx' + 1) (rowToList current) --Ova linija zamenjuje prazno polje u trenutnom redu sa diskom trenutnog igrača (C za Crveni, Z za Zuti).
    in Board (above ++ [Row newRow] ++ below)

isBoardFull :: Board Field -> Bool
isBoardFull (Board rows) = all (notElem E . rowToList) rows

-- Proveri da li je igra završena
gameEnded :: Board Field -> Bool
gameEnded board = isBoardFull board || checkWin board

-- Proveri da li je kolona puna
isColumnFull :: Int -> Board Field -> Bool
isColumnFull colIdx (Board rows) =
    let topRow = last rows
        Row fields = topRow
    in fields !! (colIdx - 1) /= E

-- Funkcija za dobijanje svih validnih poteza
validMoves :: Board Field -> [Int]
validMoves (Board rows) =
    let numCols = length (rowToList (head rows)) -- uzme prvi red table da zna koliko ima kolona
    in filter (\col -> not (isColumnFull col (Board rows))) [1 .. numCols] -- proverava da li se u svakoj koloni na vrhu nalazi E 

-- Helper function to check a sequence of four discs
isWinningSequence :: [Field] -> Bool
isWinningSequence [a, b, c, d] = a == b && b == c && c == d && a /= E
isWinningSequence _ = False  -- Handles cases where the list is shorter than 4

-- Proveri da li postoji četiri povezana diska
checkWin :: Board Field -> Bool
checkWin (Board rows) =
    let 
        checkRows = any ((any (isWinningSequence . take 4) . tails) . rowToList) rows 
        checkCols = any (any (isWinningSequence . take 4) . tails) (transpose (map rowToList rows))
        checkDiags = any (isWinningSequence . take 4) (diagonals (map rowToList rows))
    in checkRows || checkCols || checkDiags

-- Function to generate all diagonals in the board
diagonals :: [[a]] -> [[a]]
diagonals board = leftDiagonals board ++ rightDiagonals board

-- Generating left-to-right diagonals (\ direction)
leftDiagonals :: [[a]] -> [[a]]
leftDiagonals board = [ [board !! (i + k) !! (j + k) | k <- [0..3], i + k < length board, j + k < length (head board)]
                        | i <- [0..length board - 4], j <- [0..length (head board) - 4]]

-- Generating right-to-left diagonals (/ direction)
rightDiagonals :: [[a]] -> [[a]]
rightDiagonals board = [ [board !! (i + k) !! (j - k) | k <- [0..3], i + k < length board, j - k >= 0]
                         | i <- [0..length board - 4], j <- [3..length (head board) - 1]]
lista = [[E,E,E,E],[C,Z,C,Z],[C,C,Z,Z],[C,Z,Z,C]]
-- Promena igrača
changePlayer :: Player -> Player
changePlayer Crveni = Zuti
changePlayer Zuti = Crveni

-- Nasumičan izbor igrača koji igra prvi
randomFirstPlayer :: IO Player
randomFirstPlayer = do
    randomNum <- randomRIO (0, 1) :: IO Int
    return $ if randomNum == 0 then Crveni else Zuti

-- Funkcija koja simulira igru
playGame :: Board Field -> Player -> IO ()
playGame board currentPlayer = do
    printBoard board  -- Prikaži trenutnu tablu
    putStrLn $ "Na potezu je: " ++ show currentPlayer
    putStrLn "Izaberi kolonu: "
    col <- readLn  -- Pročitaj izbor kolone
    let newBoard = if col `elem` validMoves board
                   then Just (placeDisc col currentPlayer board)
                   else Nothing
    case newBoard of
        Nothing -> do
            putStrLn "Kolona je puna ili potez nije validan! Pokusaj ponovo."
            playGame board currentPlayer
        Just updatedBoard -> do
            if checkWin updatedBoard
            then do
                printBoard updatedBoard
                putStrLn $ "Igrac " ++ show currentPlayer ++ " je pobedio!"
            else if isBoardFull updatedBoard
            then do
                printBoard updatedBoard
                putStrLn "Tabla je puna, igra je neresena!"
            else do
                let nextPlayer = changePlayer currentPlayer
                playGame updatedBoard nextPlayer

-- Main funkcija koja pokreće igru
main :: IO ()
main = do
    putStrLn "Unesite broj redova: "
    rows <- readLn  -- Čitanje broja redova
    putStrLn "Unesite broj kolona: "
    cols <- readLn  -- Čitanje broja kolona
    let emptyBoard = createEmptyBoard rows cols
    firstPlayer <- randomFirstPlayer
    playGame emptyBoard firstPlayer

-- Brojanje diskova na tabli
countDiscs :: Board Field -> (Int, Int)
countDiscs (Board rows) = foldr (\row (cCount, zCount) ->
    let (c, z) = foldr (\f (cAcc, zAcc) ->
                        case f of
                          C -> (cAcc + 1, zAcc)
                          Z -> (cAcc, zAcc + 1)
                          _ -> (cAcc, zAcc)
                      ) (0, 0) (rowToList row)
    in (cCount + c, zCount + z)
  ) (0, 0) rows

-- Generisanje stabla igre
generateGameTree :: Board Field -> Int -> Rose (Board Field)
generateGameTree board 0 = Node board []  -- Base case: no more moves to simulate
generateGameTree board depth =
    if checkWin board || isBoardFull board
    then Node board []  -- No children if the game is won or the board is full
    else
        let (cCount, zCount) = countDiscs board
            currentPlayer = if cCount > zCount then Zuti else Crveni
            validMovesCols = validMoves board
            generateChildBoard col = placeDisc col currentPlayer board
            childBoards = [generateGameTree (generateChildBoard col) (depth - 1)
                           | col <- validMovesCols]
        in Node board childBoards

-- printBoard promer
-- pote = placeDIsc 2 Zuti promer
-- printBoard pote
-- checkWin pote

wins :: Board Field -> Int
wins stablo = 
    let lista = elemsOnDepth 2 (generateGameTree stablo 2)
    in length $ filter checkWin lista

boardToGameState :: Board Field -> GameState Field
boardToGameState board =
    let (cCount, zCount) = countDiscs board
        playerToMove = if cCount > zCount
                       then Zuti
                       else Crveni
    in GameState { gameBoard = board, playerToMove = playerToMove }

primer = Board [
    Row [C,Z,C,E],
    Row [Z,C,Z,E],
    Row [C,E,Z,E],
    Row [E,E,E,E]
    ]

pramer = Board [
    Row [C,Z,C,Z],
    Row [C,Z,Z,C],
    Row [Z,C,E,E],
    Row [C,Z,E,E]
    ]

promer = Board [
    Row [C,Z,C,Z,C],
    Row [C,Z,E,E,E],
    Row [E,Z,E,E,E],
    Row [E,E,E,E,E]
    ]

premer = Board [
    Row [Z,C,C,Z,Z,Z,C],
    Row [E,C,C,Z,E,Z,C],
    Row [E,Z,C,C,E,C,Z],
    Row [E,Z,E,E,E,E,C],
    Row [E,E,E,E,E,E,E],
    Row [E,E,E,E,E,E,E]
    ]

countWinningBoards :: Board Field -> Int -> Int
countWinningBoards tabla n =
    let lista = elemsOnDepth n (generateGameTree tabla n)
    in length $ filter checkWin lista


-- lista = elemsOnDepth 3 (generateGameTree primer 3)
