module Interaction where

import Types

data Direction = DLeft
               | DRight
               deriving (Eq, Show)

data Turn = Turn Direction Integer Card
          deriving (Eq, Show)

-- Returns squence of turns required to create value in certain slot
create :: Value -> Integer -> [Turn]
create (IntValue value) slot = createNumber value slot
create (CardValue value) slot = [Turn DRight slot value]
create (AppValue (CardValue c) value) slot = create value slot ++ [Turn DLeft slot c] 
create (AppValue value (CardValue c)) slot = create value slot ++ [Turn DRight slot c] 

-- create (LAppValue card value) slot = Turn DLeft slot card : create value slot
-- create (RAppValue value card) slot = Turn DRight slot card : create value slot

-- TBD. Creates numeric value in specified slot
createNumber :: Integer -> Integer -> [Turn]
createNumber value slot | value == 0 = [Turn DRight slot Zero]
                        | even value = createNumber (value `div` 2) slot
                                       ++ [Turn DLeft slot Dbl]                                       
                        | otherwise = createNumber (value - 1) slot 
                                      ++ [Turn DLeft slot Succ]

createNumberFromNumber :: Integer -> Integer -> Integer -> [Turn]
createNumberFromNumber from to slot | from == to = []
                                    | from < to = tail $ createNumber (to - from) slot
                                    | otherwise = inEmptySlot (createNumber to slot) slot

-- Puts sequence of turns in empty slot
inEmptySlot :: [Turn] -> Integer -> [Turn]
inEmptySlot turns slot = [Turn DLeft slot Put] ++ turns

data Slots a = Slots a ()

-- ...
tryReduce :: Slots Value -> Maybe (Slots Value)

-- Simple cases
tryReduce term@(Slots (IntValue _) st) = Just term
tryReduce (Slots (CardValue Zero) st) = Just (Slots (IntValue 0) st)
tryReduce term@(Slots (CardValue _) st) = Just term

--- Applications
-- Apply number == fail
tryReduce (Slots (AppValue (IntValue x) _) st) = Nothing

-- Unary cards
tryReduce (Slots (AppValue (CardValue I) value) st) = tryReduce (Slots value st)

tryReduce (Slots (AppValue (CardValue Succ) value) st) = doR rSucc value st
tryReduce (Slots (AppValue (CardValue Dbl) value) st) = doR rDbl value st
tryReduce (Slots (AppValue (CardValue Get) value) st) = doR rGet value st
tryReduce (Slots (AppValue (CardValue Put) value) st) = doR rPut value st
tryReduce (Slots (AppValue (CardValue Inc) value) st) = doR rInc value st
tryReduce (Slots (AppValue (CardValue Dec) value) st) = doR rDec value st
tryReduce (Slots (AppValue (CardValue Copy) value) st) = doR rCopy value st
tryReduce (Slots (AppValue (CardValue Revive) value) st) = doR rRevive value st

--- Binary cards
tryReduce (Slots (AppValue (AppValue (CardValue K) v1) v2) st) = doR2 rK v1 v2 st
tryReduce term@(Slots (AppValue (CardValue K) _) st) = Just term

tryReduce (Slots (AppValue (AppValue (CardValue Zombie) v1) v2) st) = doR2 rZombie v1 v2 st
tryReduce term@(Slots (AppValue (CardValue Zombie) _) st) = Just term

--- Ternary cards
tryReduce (Slots (AppValue (AppValue (AppValue (CardValue S) v1) v2) v3) st) = doR3 rS v1 v2 v3 st
tryReduce term@(Slots (AppValue (AppValue (CardValue S) _) _) st) = Just term
tryReduce term@(Slots (AppValue (CardValue S) _) st) = Just term

tryReduce (Slots (AppValue (AppValue (AppValue (CardValue Attack) v1) v2) v3) st) = doR3 rAttack v1 v2 v3 st
tryReduce term@(Slots (AppValue (AppValue (CardValue Attack) _) _) st) = Just term
tryReduce term@(Slots (AppValue (CardValue Attack) _) st) = Just term

tryReduce (Slots (AppValue (AppValue (AppValue (CardValue Help) v1) v2) v3) st) = doR3 rHelp v1 v2 v3 st
tryReduce term@(Slots (AppValue (AppValue (CardValue Help) _) _) st) = Just term
tryReduce term@(Slots (AppValue (CardValue Help) _) st) = Just term

--- FAIL CASE
tryReduce _ = undefined

doR f value st = tryReduce (Slots value st) >>= f

doR2 f v1 v2 st = do
  (Slots v1' st') <- tryReduce (Slots v1 st)
  (Slots v2' st'') <- tryReduce (Slots v2 st')
  let v1'' = (Slots v1' st'')
  let v2'' = (Slots v2' st'')
  f v1'' v2''

doR3 f v1 v2 v3 st = do
  (Slots v1' st') <- tryReduce (Slots v1 st)
  (Slots v2' st'') <- tryReduce (Slots v2 st')
  (Slots v3' st''') <- tryReduce (Slots v2 st'')
  let v1'' = (Slots v1' st''')
  let v2'' = (Slots v2' st''')
  let v3'' = (Slots v3' st''')
  f v1'' v2'' v3''

rSucc v@(Slots (IntValue n) st) | n < 65535 = Just (Slots (IntValue (n+1)) st) 
                                | otherwise = Just v
rSucc _ = Nothing

rDbl v@(Slots (IntValue n) st) | n < 32768 = Just (Slots (IntValue (n*2)) st) 
                               | otherwise = Just (Slots (IntValue 65535) st)
rDbl _ = Nothing

rGet = undefined
rPut = undefined
rInc = undefined
rDec = undefined
rCopy = undefined
rRevive = undefined
rK v1 v2 = Just v1
rZombie = undefined
rS = undefined
rAttack = undefined
rHelp = undefined

applyNumber term num | num > 0 = applyNumber (AppValue (AppValue (CardValue S) (AppValue (CardValue K) term)) (CardValue Succ)) (num - 1) 
                     | num == 0 = AppValue term (CardValue Zero)

applyTermToSlot term slotNum = applyNumber (AppValue (AppValue (CardValue S) (AppValue (CardValue K) term)) (CardValue Get)) slotNum

cA = AppValue (CardValue S) (AppValue (CardValue K) (CardValue I))

-- loopSlot is the slot where the whole loop should be built 
-- tempSlot is the temp slot for loop composing
-- action slot should contain action term without last application 
-- lastParamSlot should contain (K lastParam) term
wunderWaffle loopSlot tempSlot actionSlot lastParamSlot = 
  inEmptySlot (create (AppValue (AppValue (CardValue S) (applyTermToSlot (AppValue (CardValue S) (AppValue (CardValue K) (applyTermToSlot cA actionSlot))) lastParamSlot)) (CardValue I)) tempSlot) tempSlot 
  ++ inEmptySlot (create (applyTermToSlot (applyTermToSlot (AppValue (CardValue S) (CardValue Get)) tempSlot) loopSlot) loopSlot) loopSlot
  
killStep hp sacrifizeSlot targetSlot loopSlot (ts1, ts2, ts3) = 
  inEmptySlot (createNumber (255 - targetSlot) ts2) ts2 
  ++
  inEmptySlot (create (applyTermToSlot (AppValue (CardValue Attack) (IntValue sacrifizeSlot)) ts2) ts1) ts1
  ++
  inEmptySlot (create (AppValue (CardValue K) (IntValue hp)) ts2) ts2
  ++
  wunderWaffle loopSlot ts3 ts1 ts2
  ++
  inEmptySlot (create (AppValue (CardValue Revive) (IntValue sacrifizeSlot)) ts1) ts1
  