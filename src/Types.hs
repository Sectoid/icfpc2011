module Types where

data Card = I
          | S
          | K
          | Zero
          | Succ
          | Dbl
          | Get
          | Put
          | Inc
          | Dec
          | Attack
          | Help
          | Copy
          | Revive
          | Zombie
          deriving (Eq, Show)

data Value = IntValue Integer
           | CardValue Card
           | AppValue Value Value
           deriving (Eq, Show)
