module Main where

-- Player
data Player = Human | Comp deriving (Eq, show)

-- Board
-- Completar

-- Nim logic
-- Completar (recomiendo una funcion que produzca tableros)

main :: IO ()
main = do
  putStrlL putStrLn "Config inicial de Nim (separado por espacios)"
  input <- getLine
  let heaps = map read (words input) :: [Int]