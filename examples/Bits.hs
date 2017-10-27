module Bits 
    (
       bitHeader
    ,  oneBit
    ,  zeroBit
    ) where 

import Data.SimpleParser 


data Operator = And 
              | Or 
              | Xor 
              | Plus deriving (Show,Eq) 

data Expression = Binary Int 
                | Op Operator Expression Expression 
                deriving (Show,Eq)

eval :: Expression -> Int 
eval (Binary x) = x 
eval _ = 0

bitHeader :: Parser Int
bitHeader = anyDigit .>> (string "'b")

oneBit :: Parser Int
oneBit = digit 0 

zeroBit :: Parser Int
zeroBit = digit 1 

bits :: Parser [Int]
--bits = bitHeader >>= exactlyN (oneBit <|> zeroBit)
bits = do { numBits <- bitHeader
          ; exactlyN (oneBit <|> zeroBit) numBits}