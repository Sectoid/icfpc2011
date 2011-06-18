module SkiParser
       where

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