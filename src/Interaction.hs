module LTG.Interaction where

import LTG.Types

data Direction = DLeft
               | DRight
               deriving (Eq, Show)

data Turn = Turn Direction Integer Card
          deriving (Eq, Show)

-- Returns squence of turns required to create value in certain slot
create :: Value -> Integer -> [Turn]
create (IntValue value) slot = createNumber value slot
create (CardValue value) slot = [Turn DRight slot value]
create (LAppValue card value) slot = Turn DLeft slot card : create value slot
create (RAppValue value card) slot = Turn DRight slot card : create value slot

-- TBD. Creates numeric value in specified slot
createNumber :: Integer -> Integer -> [Turn]
createNumber value slot | value == 0 = [Turn DRight slot Zero]
                        | even value = createNumber (value `div` 2) slot
                                       ++ [Turn DLeft slot Dbl]                                       
                        | otherwise = createNumber (value - 1) slot 
                                      ++ [Turn DRight slot Succ]

createNumberFromNumber :: Integer -> Integer -> Integer -> [Turn]
createNumberFromNumber from to slot | from == to = []
                                    | from < to = tail $ createNumber (to - from) slot
                                    | otherwise = inEmptySlot (createNumber to slot) slot

-- Puts sequence of turns in empty slot
inEmptySlot :: [Turn] -> Integer -> [Turn]
inEmptySlot turns slot = [Turn DLeft slot Put] ++ turns
