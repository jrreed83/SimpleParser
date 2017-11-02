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


data Expression = Number {size::Int, val::Int}
                | Var {size::Int, name::String}
                | BinaryOp Op2 Expression Expression
                | UnaryOp Op1 Expression 
                | Get Expression Int
                deriving (Show)

data Bit = H | L deriving Show 

wire :: Parser Expression
wire = 
    do { _ <- string "wire"
       ; _ <- spaces
       ; _ <- char '['
       ; i <- anyDigit 
       ; _ <- char ':' 
       ; j <- anyDigit 
       ; _ <- char ']'     
       ; _ <- spaces 
       ; _ <- char 'x'
       ; _ <- char ';'        
       ; return (Var 3 "x") }

bitHeader :: Parser Int
bitHeader = anyDigit .>> (string "'b")

high :: Parser Bit
high = do { _ <- digit 1
          ; return H } 

low :: Parser Bit
low = do { _ <- digit 0
         ; return L }

bits2int :: [Bit] -> Int 
bits2int lst = 
     loop lst 0 0 
     where loop []    _ accum = accum 
           loop (L:t) i accum = loop t (i+1) accum
           loop (H:t) i accum = loop t (i+1) (accum + (shiftL 1 i))

--int2bit :: Int -> [Bit]
--int2bit x = 
--    loop x 0 []
--    where loop 0 _ lst = lst
--          loop y i lst = loop (shiftR y 1) i+1 (h:lst)
--                         where h = if (lst .&. 1 == 1) then H else L

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
            ; return $ Number numBits (bits2int list) }
 
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

eval :: Expression -> Int
eval (BinaryOp AND left right) = (eval left) .&. (eval right)
eval (Number _ x)                = x 

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
