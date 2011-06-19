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
           | LAppValue Card Value
           | RAppValue Value Card
           deriving (Eq, Show)
