module ARM 

where

    import Data.SimpleParser

    comma :: Parser Char 
    comma = char ',' 

    constant :: Parser Exp
    constant = 
        do { _ <- string "#"
           ; return $  Con 4}
 
 
    reg :: Int -> Parser Exp
    reg i = (string ri) >>= ( \_ -> return $ Reg ri)
          where ri = "r"++(show i)
  
    
    register :: Parser Exp
    register = (reg 0 ) <|> (reg 1 ) <|> (reg 2 ) <|> (reg 3) <|> (reg 4) <|> 
               (reg 5 ) <|> (reg 6 ) <|> (reg 7 ) <|> (reg 8) <|> (reg 9) <|> 
               (reg 10) <|> (reg 10) <|> (reg 11) 

    data Op = ADD | SUB | MOV deriving (Show)

    data Exp = Exp3 Op Exp Exp Exp 
             | Exp2 Op Exp Exp
             | Exp1 Op Exp 
             | Reg String
             | Con Int
             deriving (Show)    

    add :: Parser Exp
    add = 
        do { _   <- string "ADD" 
           ; _   <- spaces 
           ; rd  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; rn  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; arg <- constant <|> register 
           ; return $ Exp3 ADD rd rn arg } 

    sub :: Parser Exp
    sub =
        do { _   <- string "SUB" 
           ; _   <- spaces 
           ; rd  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; rn  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; arg <- constant <|> register 
           ; return $ Exp3 SUB rd rn arg } 

    mov :: Parser Exp
    mov =
        do { _   <- string "MOV" 
           ; _   <- spaces 
           ; rd  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; rn  <- register <|> constant 
           ; return $ Exp2 MOV rd rn }            