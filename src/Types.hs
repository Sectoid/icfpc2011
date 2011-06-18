module LTG.Types where

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

data Value = IntValue Integer
           | CardValue Card
           | AppValue Card Value
