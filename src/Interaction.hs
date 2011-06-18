module LTG.Interaction where

import LTG.Types

data Direction = DLeft
               | DRight

data Turn = Turn Direction Integer Card

-- Returns squence of turns required to create value in certain slot
create :: Value -> Integer -> [Turn]
create (IntValue value) slot = createNumber value slot
create (CardValue value) slot = resetSlot slot ++ [Turn DRight slot value]
create (LAppValue card value) slot = Turn DLeft slot card : create value slot
create (RAppValue value card) slot = Turn DRight slot card : create value slot

-- TBD. Creates numeric value in specified slot
createNumber :: Integer -> Integer -> [Turn]
createNumber = undefined

-- Resets slot value
resetSlot :: Integer -> [Turn]
resetSlot slotNumber = [Turn DLeft slotNumber Put]