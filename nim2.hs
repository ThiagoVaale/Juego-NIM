module Main where

import Data.Char (isSpace)
import Data.List (intercalate)

--player
data Player = A | B deriving (Eq, Show)

other :: Player -> Player
other A = B
other B = A

--board
type Position = [Int]

isTerminal :: Position -> Bool
isTerminal = all (== 0)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs =
  let (as, _ : bs) = splitAt i xs
  in as ++ x : bs

--nim logic
allNextMoves :: Position -> [Position]
allNextMoves piles =
  [ replaceAt i (k - t) piles
  | (i, k) <- zip [0..] piles
  , k > 0
  , t <- [1..k]
  ]

solveGameFor :: Player -> Position -> Player
solveGameFor me pos
  | isTerminal pos = me
  | otherwise =
      let opponent = other me
          nextPositions = allNextMoves pos
          endgames = map (solveGameFor opponent) nextPositions
      in if me `elem` endgames
           then me
           else opponent

bestMoveFor :: Player -> Position -> Maybe (Int, Int)
bestMoveFor me piles
  | isTerminal piles = Nothing
  | otherwise =
      let opponent = other me
          candidates =
            [ (i, t, replaceAt i (k - t) piles)
            | (i, k) <- zip [0..] piles
            , k > 0
            , t <- [1..k]
            ]
          wins = [ (i, t) | (i, t, pos') <- candidates
                          , solveGameFor opponent pos' == me ]
      in case wins of
           (i,t):_ -> Just (i, t)        -- primera que fuerza victoria
           []      -> Nothing            -- no hay forma de forzar victoria

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

parsePosition :: String -> [Int]
parsePosition s = map read (words s)

prettyPosition :: Position -> String
prettyPosition = intercalate " " . map show

main :: IO ()
main= do
  putStrLn "Nim ingresa las pilas separadas por espacios (ej.: 3 4 5):"
  line <- getLine
  let pos = parsePosition line
      starter = A
      winner  = solveGameFor starter pos
  putStrLn $ "Posicion inicial: [" ++ prettyPosition pos ++ "]"
  putStrLn $ "Con juego perfecto, el ganador sera: " ++ show winner
  case bestMoveFor starter pos of
        Just (i, t) ->
          putStrLn $ "Sugerencia para " ++ show starter ++
                     ": quitar " ++ show t ++ " de la pila #" ++ show i
        Nothing ->
          putStrLn $ "No hay jugada que garantice victoria para " ++ show starter ++
                     " (la posicion es ganadora para " ++ show (other starter) ++ ")."