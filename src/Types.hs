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
          deriving (Eq)

data Value = IntValue Integer
           | CardValue Card
           | AppValue Value Value
           deriving (Eq, Show)

instance Show Card where 
  show I = "I"
  show S = "S"
  show K = "K"
  show Zero = "zero"
  show Succ = "succ"
  show Dbl = "dbl"
  show Get = "get"
  show Put = "put"
  show Inc = "inc"
  show Dec = "dec"
  show Attack = "attack"
  show Help = "help"
  show Copy = "copy"
  show Revive = "revive"
  show Zombie = "zombie"
