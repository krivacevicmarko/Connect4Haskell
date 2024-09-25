import Module.StateMonada
import Module.Connect4
import Module.Rose
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Control.Monad (forM)

-- Print the parsed board for debugging
printParsedBoard :: [[Char]] -> IO ()
printParsedBoard = mapM_ putStrLn . map concat . map (map showF)
  where showF 'E' = "E"
        showF 'C' = "C"
        showF 'Z' = "Z"

-- Parse a single row of the board, handling vertical bars and spaces
-- Parse a single row of the board, handling vertical bars and spaces
parseRow :: String -> Either String [Char]
parseRow row =
  let fields = wordsWhen (=='|') row -- Split the row by vertical bars "|"
      parseField field
        | field == "   " = Right 'E' -- Empty field
        | field == " C " = Right 'C' -- Red player's disc
        | field == " Z " = Right 'Z' -- Yellow player's disc
        | otherwise      = Left "Neispravno formatirano polje"
  in if head row == '|' && last row == '|'  -- Ensure row starts and ends with '|'
       then traverse parseField fields  -- Parse all fields, including first and last
       else Left "Neispravno formatiran fajl (nema pocetnog ili zavrsnog |)"


-- Utility to split string by a delimiter
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
        where (w, s'') = break p s'

-- Parse the board (list of rows)
parseBoard :: [String] -> Either String [[Char]]
parseBoard = traverse parseRow

-- Parse the moves (remaining lines with numeric columns)
parseMoves :: [String] -> Either String [Int]
parseMoves = traverse parseMove
  where
    parseMove s
      | all isDigit s = Right (read s :: Int)
      | otherwise = Left "Neispravno formatiran potez"

-- Top-level function to parse the entire game file
parseGameFile :: String -> Either String ([[Char]], [Int])
parseGameFile content =
  let rows = lines content
      (boardRows, moveRows) = span (\row -> '|' `elem` row) rows -- Separate board and moves
  in do
    board <- parseBoard boardRows
    moves <- parseMoves (filter (not . null) moveRows)  -- Filter out empty rows
    return (board, moves)

-- Convert a character 'C', 'Z', or 'E' into the Field type
charToField :: Char -> Field
charToField 'C' = C
charToField 'Z' = Z
charToField 'E' = E

-- Convert parsed board (list of strings) into the Board structure
parsedBoardToBoardState :: [[Char]] -> Board Field
parsedBoardToBoardState parsedBoard = listToBoard $ map (map charToField) (reverse parsedBoard)

-- Example top-level function that integrates everything
runGameWithParsedData :: ([[Char]], [Int]) -> IO ()
runGameWithParsedData (parsedBoard, parsedMoves) = do
    -- Convert the parsed board into the BoardState
    let initialBoard = parsedBoardToBoardState parsedBoard
        initialState = BoardState {
            gameBoard = initialBoard,
            playerToMove = determineNextPlayer initialBoard,
            gameOver = checkWin initialBoard,
            errorMsg = if checkWin initialBoard
                       then Just "Igra je već završena."
                       else Nothing
        }
    printBoard (gameBoard initialState)
    -- Run the moves through the game logic
    let (result, finalState) = runGameState (applyMovesFromList parsedMoves) initialState
    -- Print the result of applying moves
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right _  -> do
            putStrLn "Updated Board:"
            printBoard (gameBoard finalState)

main :: IO ()
main = do
  content <- readFile "tabla.txt"
  case parseGameFile content of
    Left err -> putStrLn $ "Error: " ++ err
    Right (parsedBoard, parsedMoves) -> do
      putStrLn "Parsed board:"
      printParsedBoard parsedBoard
      putStrLn "Parsed moves:"
      print parsedMoves
      runGameWithParsedData (parsedBoard, parsedMoves)