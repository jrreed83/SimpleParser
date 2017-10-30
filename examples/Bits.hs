module Bits 
    (
       bitHeader
    ,  oneBit
    ,  zeroBit
    ) where 

import Data.SimpleParser 


data Operator2 = AND 
               | OR 
               | XOR 
               deriving (Show) 

data Expression = Number [Int] 
                | Op Operator2 Expression Expression 
                deriving (Show)

eval :: Expression -> [Int]
eval (Op AND lhs rhs) = (eval lhs) |&| (eval rhs)
eval (Op OR  lhs rhs) = (eval lhs) |&| (eval rhs)
eval (Op XOR lhs rhs) = (eval lhs) |&| (eval rhs)
eval (Number x)       = x

(|&|) :: [Int] -> [Int] -> [Int]
x |&| y = x 

bitHeader :: Parser Int
bitHeader = anyDigit .>> (string "'b")

oneBit :: Parser Int
oneBit = digit 0 

zeroBit :: Parser Int
zeroBit = digit 1 

bit :: Parser Int
bit = oneBit <|> zeroBit

andToken :: Parser Operator2 
andToken = do { _ <- char '&'
              ; return AND}

orToken :: Parser Operator2 
orToken = do { _ <- char '|'
             ; return OR}

xorToken :: Parser Operator2 
xorToken = do { _ <- char '^'
              ; return XOR}             

token :: Parser Operator2 
token = andToken <|> orToken <|> xorToken


number :: Parser Expression
--bits = bitHeader >>= exactlyN (oneBit <|> zeroBit)
number = do { numBits <- bitHeader
            ; list <- exactlyN bit numBits
            ; return $ Number list }
 
foo :: Parser Expression
foo = do { x <- number 
         ; spaces
         ; t <- token
         ; spaces 
         ; y <- number
         ; return $ Op t x y}