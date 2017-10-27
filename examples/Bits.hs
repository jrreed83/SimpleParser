module Bits 
    (
       bitHeader
    ,  oneBit
    ,  zeroBit
    ) where 

import Data.SimpleParser 

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