module Nim where

import Data.Bits (xor)

type Piles = [Int] 

data Player = A | B deriving (Eq, Show)

other :: Player -> Player
other A = B
other B = A

nimSum :: Piles -> Int
nimSum = foldl xor 0

winnerFrom :: Player -> Piles -> Player
winnerFrom who piles = 
    if nimSum piles == 0
        then other who
        else who

winner :: Piles -> Player
winner = winnerFrom A

nimWinner :: Piles -> String
nimWinner piles = 
    case winner piles of
        A -> "Gana A"
        B -> "Gana B"

