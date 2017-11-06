module ARM 

where

    import Data.SimpleParser

    comma :: Parser Char 
    comma = char ',' 

    decimal :: Parser Exp
    decimal = 
        do { _ <- string "#"
           ; d <- anyDigit
           ; return $  Dec d}
 
 
    register :: Parser Exp 
    register = 
        do { _ <- char 'r' 
           ; i <- integer 
           ; return $ Reg i }
  
    --register :: Parser Ast
    --register = (reg 0 ) <|> (reg 1 ) <|> (reg 2 ) <|> (reg 3) <|> (reg 4) <|> 
    --           (reg 5 ) <|> (reg 6 ) <|> (reg 7 ) <|> (reg 8) <|> (reg 9) <|> 
    --           (reg 10) <|> (reg 10) <|> (reg 11) 

    data Exp = Add    Exp Exp Exp
             | Sub    Exp Exp Exp
             | Move   Exp Exp 
             | Dref   Exp Exp
             | Load   Exp Exp 
             | Store  Exp Exp
             | Reg    Int
             | Dec    Int
             deriving (Show)    
    
    eol :: Parser String 
    eol = (string "\n") <|> spaces 

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
           ; arg <- decimal <|> register 
           ; return $ Add rd rn arg } 

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
           ; arg <- decimal <|> register 
           ; return $ Sub rd rn arg } 

    mov :: Parser Exp
    mov =
        do { _   <- string "MOV" 
           ; _   <- spaces 
           ; rd  <- register 
           ; _   <- comma 
           ; _   <- spaces 
           ; rn  <- register <|> decimal 
           ; return $ Move rd rn }            
    
    derefOffset :: Parser Exp 
    derefOffset = 
        do { _      <- char '['
           ; rn     <- register
           ; _      <- comma 
           ; _      <- spaces
           ; offset <- decimal 
           ; _      <- char ']'
           ; return $ Dref rn offset } 

    derefNoOffset :: Parser Exp 
    derefNoOffset = 
        do { _      <- char '['
           ; rn     <- register  
           ; _      <- char ']'
           ; return $ Dref rn (Dec 0) } 

    deref :: Parser Exp 
    deref = derefOffset <|> derefNoOffset  
    
    ldr :: Parser Exp
    ldr = 
        do { _  <- string "LDR"
           ; _  <- spaces
           ; rd <- register
           ; _  <- comma 
           ; _  <- spaces 
           ; d  <- deref 
           ; return $ Load rd d }

    str :: Parser Exp
    str = 
        do { _  <- string "STR"
           ; _  <- spaces
           ; rn <- (label "expect register" register)
           ; _  <- comma 
           ; _  <- spaces 
           ; d  <- deref 
           ; return $ Store rn d }

    parse :: Parser Exp 
    parse = do 
             exp <- mov <|> add <|> sub <|> str <|> ldr
             _   <- eol 
             return exp

    parseAll :: String -> Either String [Exp]
    parseAll lines =
        parseAll' lines []
        where parseAll' []    acc = Right acc
              parseAll' lines acc =  
                   case run parse lines of 
                        Failure msg -> Left msg 
                        Success x r -> parseAll' r (acc ++ [x]) 
              

    getAst :: String -> IO (Either String [Exp]) 
    getAst fileName = do 
          contents <- readFile fileName 
          return $ parseAll contents
                            


    