module LTG.Interaction where

import LTG.Types

data Direction = Left
               | Right

data Turn = Turn Direction Integer Card
