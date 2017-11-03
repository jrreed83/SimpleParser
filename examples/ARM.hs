module ARM 

where

    import Data.SimpleParser

    comma :: Parser Char 
    comma = char ',' 

    r0 :: Parser Arg
    r0 = 
        do { _ <- string "r0" 
           ; return R0     }

    r1 :: Parser Arg
    r1 = 
        do { _ <- string "r1" 
           ; return R1     }

    r2 :: Parser Arg
    r2 = 
        do { _ <- string "r2" 
           ; return R2     }

    r3 :: Parser Arg
    r3 = 
        do { _ <- string "r3" 
           ; return R3     }

    immediate :: Parser Arg 
    immediate = 
        do { _ <- string "#"
           ; return $ Immediate 4}

    register :: Parser Arg 
    register = r0 <|> r1 <|> r2 <|> r3


    data Op = ADD | SUB deriving (Show)

    data Arg = R0 | R1 | R2 | R3 | Immediate Int 
               deriving (Show)

    data Expression =  ALU Op Arg Arg Arg deriving (Show)

    add :: Parser Expression 
    add = 
        do { _   <- string "ADD" 
           ; _   <- spaces 
           ; rd  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; rn  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; arg <- immediate <|> register 
           ; return $ ALU ADD rd rn arg } 