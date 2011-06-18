module SkiParser
       where

import qualified Data.Sequence as S

import Control.Monad.ST
import Data.Array.ST

import Types
import Interaction

makeArray :: Int -> Int -> a -> ST s (STArray s Int a)
makeArray lower upper val = newArray (lower, upper) val

cells = makeArray 0 255 (10000, CardValue I)

update = do arr <- cells
            a <- readArray arr 1
            writeArray arr 1 (5000, CardValue I)
            b <- readArray arr 1
            return (a, b)

data Function a b c d = Value   a
                      | Unary   (a -> b)
                      | Binary  (a -> b -> c)
                      | Ternary (a -> b -> c -> d)
                        
apply (Unary f)   x = Value  (f x)
apply (Binary f)  x = Unary  (f x)
apply (Ternary f) x = Binary (f x)

i :: a -> a
i = id

zero :: Integer
zero = 0

succ :: Integer -> Integer
succ = (+1)

dbl :: Integer -> Integer
dbl = (*2)

-- State
get :: Int -> Value
get n = snd $ runST ( do arr <- cells
                         readArray arr n
                    )

put :: a -> (a -> a)
put _ = i

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k = const

inc :: Int -> (a -> a)
inc n = runST ( do arr <- cells
                   (vit, val) <- readArray arr n
                   writeArray arr n (vit + 1, val)
                   return i
              )

dec :: Int -> (a -> a)
dec n = runST ( do arr <- cells
                   (vit, val) <- readArray arr n
                   writeArray arr n (vit - 1, val)
                   return i
              )
-- attack
-- help
-- copy

revive' :: Int -> Int
revive' n | n <= 0    = 1
          | otherwise = n

revive :: Int -> (a -> a)
revive n = runST ( do arr <- cells
                      (vit, val) <- readArray arr n
                      writeArray arr n (revive' vit, val)
                      return i
              )


-- zombie


main :: IO ()
main = print $ runST update