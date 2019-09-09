{-
    COMP30030 Project 1 - Card Guessing Game
    Arthur: Yunfei Jing 987784

    The aim of this project is to guess a set of selected cards, given the 
    feedback of each guess, and try to make least guess based on the feedbacks.
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List


{- 
    GameState type can store the list of all the remaining possible answers.
-}
type GameState = [[Card]]


{- 
    feedback takes a target and a guess, each represented as a list of Cards.
    Return the five feedback numbers as a tuple.
    Where each number represented as the number of cards in the answer such that:
    1. Also in the guess
    2. Have rank lower than the lowest rank in the guess
    3. Have the same rank as a card in the guess
    4. Have rank higher than the highest rank in the guess
    5. Have the same guit as a card in the guess
-}
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback answer guess = (i1,i2,i3,i4,i5)
    where   i1 = correctCards answer guess
            i2 = lowerRanks answer guess
            i3 = correctRanks answer guess
            i4 = higherRanks answer guess
            i5 = correctSuits answer guess


{-
    initialGuess takes the number of cards in the answer as input and returns a 
    pair of an initial guess, which should be a list of the specified number of 
    cards, and a game state.
-}
initialGuess :: Int -> ([Card],GameState)
initialGuess n
    | (n<2 || n>4) = error "Only 2,3,4 cards guessing is allowed."
    | otherwise = (initialGuessGenerator n, combinations n deck)
    where deck = ([minBound..maxBound] :: [Card])


{-
  nextGuess takes as input a pair of the previous guess and game state, and the
  feedback to this guess, and returns a pair of the next guess
  and a new game state.
-}
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (oldGuess, oldGS) fb = (newGuess, newGS)
    where
          newGuess = newGS!!0
          -- The new game state is all the combinations have the same feeback
          -- as the previoud guess, if we assume that card is the answer
          newGS = [x | x <- oldGS, (feedback x oldGuess)==fb]


-- Compute the number of exact same cards in both answer and guess
correctCards :: [Card] -> [Card] -> Int
correctCards ans gus = length (ans `intersect` gus)


-- Compute the number of cards in the answer have rank lower than the lowest 
-- card in the guess 
lowerRanks :: [Card] -> [Card] -> Int
lowerRanks ans gus = length (filter (<minRank) (map rank ans))
    where
            minRank = minimum (map rank gus)


-- Compute the number of cards in the answer have the same rank in the guess
correctRanks :: [Card] -> [Card] -> Int
correctRanks ans gus = min (length ((rankA) `intersect` (rankG)))
                           (length ((rankG) `intersect` (rankA)))
    where 
            rankA = map rank ans
            rankG = map rank gus


-- Compute the number of cards in the answer have rank higher than the highest
-- card in the guess
higherRanks :: [Card] -> [Card] -> Int
higherRanks ans gus = length (filter (>maxRank) (map rank ans))
    where
            maxRank = maximum (map rank gus)


-- Compute the number of cards in the answer have the same suit in the guess
correctSuits :: [Card] -> [Card] -> Int
correctSuits ans gus = min (length ((suitA) `intersect` (suitG))) 
                           (length ((suitG) `intersect` (suitA)))
    where 
            suitA = map suit ans
            suitG = map suit gus


-- Generate the first guess using the optimized method (hint 4)
-- Using hardcoding since only 2,3,4 cards guessing is allowed
initialGuessGenerator :: Int -> [Card]
initialGuessGenerator n
    | n == 2 = [Card Club R5, Card Diamond R9]
    | n == 3 = [Card Club R4, Card Heart R7, Card Spade R10]
    | n == 4 = [Card Club R3, Card Diamond R5, Card Heart R7, Card Spade R9]


-- Compute all the unique combinations of size n in a give list
-- Source: https://wiki.haskell.org/99_questions/Solutions/26
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                            , ys <- combinations (n-1) xs']