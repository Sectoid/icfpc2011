module SkiParser
       where

data FApp a b = FApp1 (a -> b) a
              | FApp2 (a -> b -> c) a

apply :: (FApp a b) -> b
apply (FApp1 f x) = f x
apply (FApp2 f x) = f x

--f :: a -> b
--f = s

i :: a -> a
i = id

zero :: Integer
zero = 0

succ :: Integer -> Integer
succ = (+1)

dbl :: Integer -> Integer
dbl = (*2)

-- get

put :: a -> (a -> a)
put _ = i

s :: (a -> b -> c) -> (a -> b) -> a -> c
s f g x = f x (g x)

k :: a -> b -> a
k = const

-- inc
-- dec
-- attack
-- help
-- copy
-- revive
-- zombie


main :: IO ()
main = print ""