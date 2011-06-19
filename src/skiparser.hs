module SkiParser
       where

import qualified Data.Sequence as S

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Text.Regex.Posix

import Types
import Interaction

type Action s a = STArray s Int (Int, Value) -> ST s (a -> a)

-- Not sure actually is there any sense using regex for an exact match
matchCard :: String -> Card -> Bool
matchCard s c = s =~ matchCard' c
  where matchCard' c = '^' : show c ++ "$"

parseCard :: String -> Card
parseCard s | matchCard s I      = I
            | matchCard s S      = S
            | matchCard s K      = K
            | matchCard s Zero   = Zero
            | matchCard s Succ   = Succ
            | matchCard s Dbl    = Dbl
            | matchCard s Get    = Get
            | matchCard s Put    = Put
            | matchCard s Inc    = Inc
            | matchCard s Dec    = Dec
            | matchCard s Attack = Attack
            | matchCard s Help   = Help
            | matchCard s Copy   = Copy
            | matchCard s Revive = Revive
            | matchCard s Zombie = Zombie
            | otherwise = error "Card type not supported"

-- Interaction with the server needed for the submitting tuns list
walkPath :: [Turn] -> IO ()
walkPath turns = mapM_ walkTurn turns

-- Stub. It should really read the opponent's commands and choose adequate strategy
readTurn :: IO ()
readTurn = do rl   <- getInt
              card <- getLine
              i    <- getLine
              return ()
              
readTurn' :: IO ()
readTurn' = return ()

walkTurn :: Turn -> IO ()
walkTurn (Turn dir x y) | dir == DLeft  = do walkL x y
                                             readTurn'
                        | dir == DRight = do walkR x y
                                             readTurn'
                        | otherwise = error "Turn type not supported"

walkL, walkR :: Integer -> Card -> IO ()
walkL i card = do putStrLn "1"
                  putStrLn $ show card
                  putStrLn $ show i

walkR i card = do putStrLn "2"
                  putStrLn $ show i
                  putStrLn $ show card

getInt :: IO Int
getInt = readLn

loop' :: Int -> IO (Card, Int)
loop' n | n == 1 = do card <- getLine
                      slot <- getInt
                      return (parseCard card, slot) -- Apply state here
        | n == 2 = do slot <- getInt
                      card <- getLine
                      return (parseCard card, slot) -- Apply state here
        | otherwise = error "Operation type not supported"

loop :: IO (Int, Card, Int)
loop = do
  lr   <- getInt
  (card, slot) <- loop' lr
  return (lr, card, slot) -- Since we are applying state in loop', loop here

makeArray :: Int -> Int -> a -> ST s (STArray s Int a)
makeArray lower upper val = newArray (lower, upper) val

-- Initial state for the mutable boxed STArray
cells = makeArray 0 255 (10000, CardValue I)

-- Evaluation of some of the combinators
i :: a -> a
i = id

zero :: Integer
zero = 0

succ :: Integer -> Integer
succ = (+1)

dbl :: Integer -> Integer
dbl = (*2)

-- Maybe should be done in ST too, nevermind in doesn't write
get :: Int -> Value
get n = snd $ runST ( do arr <- cells
                         readArray arr n
                    )
        
getVit :: Int -> STArray s Int (Int, Value) -> ST s Int
getVit n arr = do val <- readArray arr n
                  return (fst val)
                  
setVit :: Int -> Int -> STArray s Int (Int, Value) -> ST s ()
setVit n vit arr = do (vit', val) <- readArray arr n
                      writeArray arr n (vit, val)
               
getVal :: Int -> STArray s Int (Int, Value) -> ST s Value
getVal n arr = do val <- readArray arr n
                  return (snd val)
                  
setVal :: Int -> Value -> STArray s Int (Int, Value) -> ST s ()
setVal n val arr = do (vit, val') <- readArray arr n
                      writeArray arr n (vit, val)

put :: a -> (a -> a)
put _ = i

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k = const
        
inc :: Int -> Action s a
inc n arr = do (vit, val) <- readArray arr n
               writeArray arr n (vit + 1, val)
               return i

dec :: Int -> Action s a
dec n arr = do (vit, val) <- readArray arr n
               writeArray arr n (vit - 1, val)
               return i
               
-- attack

help :: Int -> Int -> Int -> Action s a
help i j n arr = do (vit, val) <- readArray arr i
                    writeArray arr i (vit - n, val)
                    (vit', val') <- readArray arr j
                    writeArray arr j (vit + (n * 10) `div` 11, val)
                    return id

-- copy

revive :: Int -> Action s a
revive n arr = do (vit, val) <- readArray arr n
                  writeArray arr n (revive' vit, val)
                  return i
  where revive' n | n <= 0    = 1
                  | otherwise = n

-- zombie

-- Sample mutating process. Actually this will be in IO rather than ST
compute = runST ( do arr <- cells :: ST s (STArray s Int (Int, Value))
                     inc 10 arr
                     inc 10 arr
                     setVit 10 0 arr
                     revive 10 arr
                     readArray arr 10
                )

main :: IO ()
main = walkPath $ createNumber 2 1