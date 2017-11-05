module ARM 

where

    import Data.SimpleParser

    comma :: Parser Char 
    comma = char ',' 

    decimal :: Parser Ast
    decimal = 
        do { _ <- string "#"
           ; d <- anyDigit
           ; return $  Dec d}
 
 
    reg :: Int -> Parser Ast
    reg i = (string ri) >>= ( \_ -> return $ Reg ri)
          where ri = "r"++(show i)
  
    
    register :: Parser Ast
    register = (reg 0 ) <|> (reg 1 ) <|> (reg 2 ) <|> (reg 3) <|> (reg 4) <|> 
               (reg 5 ) <|> (reg 6 ) <|> (reg 7 ) <|> (reg 8) <|> (reg 9) <|> 
               (reg 10) <|> (reg 10) <|> (reg 11) 

    data Ast = ADD  Ast Ast Ast 
             | SUB  Ast Ast Ast
             | MOV  Ast Ast 
             | DREF Ast Ast
             | Reg  String
             | Dec  Int
             deriving (Show)    

    add :: Parser Ast
    add = 
        do { _   <- string "ADD" 
           ; _   <- spaces 
           ; rd  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; rn  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; arg <- decimal <|> register 
           ; return $ ADD rd rn arg } 

    sub :: Parser Ast
    sub =
        do { _   <- string "SUB" 
           ; _   <- spaces 
           ; rd  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; rn  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; arg <- decimal <|> register 
           ; return $ SUB rd rn arg } 

    mov :: Parser Ast
    mov =
        do { _   <- string "MOV" 
           ; _   <- spaces 
           ; rd  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; rn  <- register <|> decimal 
           ; return $ MOV rd rn }            
    
    derefOffset :: Parser Ast 
    derefOffset = 
        do { _      <- char '['
           ; rn     <- register
           ; _      <- comma 
           ; _      <- spaces
           ; offset <- decimal 
           ; _      <- char ']'
           ; return $ DREF rn offset } 

    derefNoOffset :: Parser Ast 
    derefNoOffset = 
        do { _      <- char '['
           ; rn     <- register  
           ; _      <- char ']'
           ; return $ DREF rn (Dec 0) } 

    deref :: Parser Ast 
    deref = derefOffset <|> derefNoOffset  
    
    parse :: Parser Ast 
    parse = mov <|> add <|> sub 
