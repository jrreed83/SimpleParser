module Bits 
    (
       bitHeader
    ,  high
    ,  low
    ) where 

import Data.SimpleParser 
import Data.Bits 

data Op2 = AND 
         | OR 
         | XOR 
         | PLUS
         | ASSIGN
         | GET
         deriving (Show) 

data Op1 = NEG deriving (Show)

data Expression = Number Int
                | Var String
                | BinaryOp Op2 Expression Expression
                | UnaryOp Op1 Expression 
                | Get Expression Int
                deriving (Show)

data Bit = H | L deriving Show 

--(.|.)  
bitHeader :: Parser Int
bitHeader = anyDigit .>> (string "'b")

high :: Parser Bit
high = do { _ <- digit 1
          ; return H } 

low :: Parser Bit
low = do { _ <- digit 0
         ; return L }

bits2int :: [Bit] -> Int 
bits2int _ = 42 

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

assignToken :: Parser Op2 
assignToken = do { _ <- char '=' 
                 ; return ASSIGN}

token2 :: Parser Op2
token2 = andToken <|> orToken <|> xorToken <|> assignToken 

token1 :: Parser Op1
token1 = negToken  

number :: Parser Expression
number = do { numBits <- bitHeader
            ; list    <- exactlyN (low <|> high) numBits
            ; return $ Number (bits2int list) }
 
leftParan :: Parser Char 
leftParan = char '('

rightParan :: Parser Char 
rightParan = char ')'

binaryExpression :: Parser Expression
binaryExpression = 
    do { _ <- leftParan
       ; x <- expression
       ; _ <- spaces
       ; t <- token2
       ; _ <- spaces 
       ; y <- expression
       ; _ <- rightParan
       ; return $ BinaryOp t x y }


--variable :: Parser Expression 
--variable = do { _ <- string "let"
--              ; _ <- spaces
--              ; x <-  
--              ; _ <- spaces 
--              ; return Var x }

unaryExpression :: Parser Expression
unaryExpression = 
    do { t <- token1
       ; x <- number
       ; return $ UnaryOp t x }      
       

expression :: Parser Expression 
expression = number <|> binaryExpression <|> unaryExpression
