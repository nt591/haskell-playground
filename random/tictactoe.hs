module Main where
  import System.Environment
  import Data.List

  data Move = X | O
  data Cell = Occupied Move | Empty
  data CellTransform = Success [Cell] | Fail String [Cell]

  instance Show Move where
    show X = "X"
    show O = "O"

  instance Show Cell where
    show (Occupied X)     = "X"
    show (Occupied O)    = "O"
    show Empty            = " "

  instance Eq Cell where
    Occupied X == Occupied X = True
    Occupied O == Occupied O = True
    Empty == Empty           = True
    _ == _                   = False

  nextMove :: Move -> Move
  nextMove X = O
  nextMove O = X

  renderRow :: [Cell] -> String
  renderRow row = intercalate " | " $ fmap show row

  dividingLine :: String
  dividingLine = "----------"

  renderBoard :: [Cell] -> IO ()
  renderBoard board = do
    putStrLn $ renderRow firstRow
    putStrLn dividingLine
    putStrLn $ renderRow secondRow
    putStrLn dividingLine
    putStrLn $ renderRow thirdRow
    where firstRow  = take 3 board
          secondRow = drop 3 . take 6 $ board
          thirdRow  = drop 6 board

  getBoardIndex :: String -> Maybe Int
  getBoardIndex "A1" = Just 0
  getBoardIndex "A2" = Just 1
  getBoardIndex "A3" = Just 2
  getBoardIndex "B1" = Just 3
  getBoardIndex "B2" = Just 4
  getBoardIndex "B3" = Just 5
  getBoardIndex "C1" = Just 6
  getBoardIndex "C2" = Just 7
  getBoardIndex "C3" = Just 8
  getBoardIndex _    = Nothing

  verifyIsFree ::  [Cell] -> Int -> Maybe Int
  verifyIsFree board ix = if board !! ix == Empty then Just ix else Nothing

  assignCell :: String -> Move -> [Cell] -> CellTransform
  assignCell location move board =
    case getBoardIndex location >>= verifyIsFree board of
      Nothing -> Fail "Invalid move" board
      Just i -> Success ((take i board) ++ [Occupied move] ++ (drop (i+1) board))

  isThereAWinner :: Move -> [Cell] -> Bool
  isThereAWinner move board =
    or [
      -- check top row
      board !! 0 == (Occupied move) && board !! 1 == (Occupied move) && board !! 2 == (Occupied move),
      -- check middle row
      board !! 3 == (Occupied move) && board !! 4 == (Occupied move) && board !! 5 == (Occupied move),
      -- check bottom row
      board !! 6 == (Occupied move) && board !! 7 == (Occupied move) && board !! 8 == (Occupied move),
      -- check left column
      board !! 0 == (Occupied move) && board !! 3 == (Occupied move) && board !! 6 == (Occupied move),
      -- check middle column
      board !! 1 == (Occupied move) && board !! 4 == (Occupied move) && board !! 7 == (Occupied move),
      -- check right column
      board !! 2 == (Occupied move) && board !! 5 == (Occupied move) && board !! 8 == (Occupied move),
      -- check top left -> bottom right
      board !! 0 == (Occupied move) && board !! 4 == (Occupied move) && board !! 8 == (Occupied move),
      -- check bottom left -> top right
      board !! 6 == (Occupied move) && board !! 4 == (Occupied move) && board !! 2 == (Occupied move)
    ]

  playRound :: Move  -> [Cell] -> IO ()
  playRound move board = do
    putStrLn $ "Pick a cell from A1 to C3."
    renderBoard board
    putStr "\nInput: "
    cell <- getLine
    case assignCell cell move board of
      Fail err board -> do
        putStrLn err
        playRound move board
      Success newBoard -> do
        if isThereAWinner move newBoard then do
          putStrLn $ ("Winner! " ++ (show move) ++ " has won!")
          renderBoard newBoard
          return ()
        else playRound (nextMove move) newBoard

  main :: IO ()
  main = do
    putStrLn $ "The game is beginning."
    putStrLn $ "X goes first."
    let newBoard = replicate 9 Empty
    playRound X newBoard