module Bits 
    (
       bitHeader
    ,  high
    ,  low
    ) where 

import Data.SimpleParser 

data Op2 = AND 
         | OR 
         | XOR 
         | PLUS
          deriving (Show) 

data Op1 = NEG deriving (Show)

data Expression = Number [Bit]
                | BinaryOp Op2 Expression Expression
                | UnaryOp Op1 Expression 
                deriving (Show)

data Bit = H | L deriving Show 

--eval :: Expression -> Int
--eval (Op AND lhs rhs) = (eval lhs) .&. (eval rhs)
--eval (Op OR  lhs rhs) = (eval lhs) .|.  (eval rhs)
--eval (Op XOR lhs rhs) = (eval lhs) `xor` (eval rhs)
--eval (Number x _)       = x

--(.|.)  
bitHeader :: Parser Int
bitHeader = anyDigit .>> (string "'b")

high :: Parser Bit
high = do { _ <- digit 1
          ; return H } 

low :: Parser Bit
low = do { _ <- digit 0
         ; return L }

bit :: Parser Bit
bit = low <|> high

andToken :: Parser Op2 
andToken = do { _ <- char '&'
              ; return AND}

orToken :: Parser Op2 
orToken = do { _ <- char '|'
             ; return OR}

xorToken :: Parser Op2 
xorToken = do { _ <- char '^'
              ; return XOR}             

negToken :: Parser Op1 
negToken = do { _ <- char '~'
              ; return NEG}    

token2 :: Parser Op2
token2 = andToken <|> orToken <|> xorToken

token1 :: Parser Op1
token1 = negToken 

number :: Parser Expression
number = do { numBits <- bitHeader
            ; list    <- exactlyN bit numBits
            ; return $ Number list }
 
binaryExpression :: Parser Expression
binaryExpression = 
    do { x <- number 
       ; _ <- spaces
       ; t <- token2
       ; _ <- spaces 
       ; y <- number
       ; return $ BinaryOp t x y }

unaryExpression :: Parser Expression
unaryExpression = 
    do { t <- token1
       ; _ <- spaces 
       ; x <- number
       ; return $ UnaryOp t x }       