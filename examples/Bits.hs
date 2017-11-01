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
         | ASSIGN
         | GET
         deriving (Show) 

data Op1 = NEG deriving (Show)

data Expression = Number [Bit]
                | Var String
                | BinaryOp Op2 Expression Expression
                | UnaryOp Op1 Expression 
                | Get Expression Int
                deriving (Show)

data Bit = H | L deriving Show 

eval :: Expression -> Int
eval (Get (Number l) i) = 
    case l !! i of 
        L -> 0
        H -> 1 
eval _ = 43 


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

assignToken :: Parser Op2 
assignToken = do { _ <- char '=' 
                 ; return ASSIGN}

token2 :: Parser Op2
token2 = andToken <|> orToken <|> xorToken <|> assignToken 

token1 :: Parser Op1
token1 = negToken  

number :: Parser Expression
number = do { numBits <- bitHeader
            ; list    <- exactlyN bit numBits
            ; return $ Number list }
 
binaryExpression :: Parser Expression
binaryExpression = 
    do { _ <- char '~'
       ; x <- expression
       ; _ <- spaces
       ; t <- token2
       ; _ <- spaces 
       ; y <- expression
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
expression = number <|> binaryExpression
