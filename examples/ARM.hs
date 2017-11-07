module ARM 

where

    import Data.SimpleParser
    import Data.IORef 

    plus :: IORef Int -> IORef Int -> IORef Int -> IO () 
    plus x y z = 
        do { y' <- readIORef y 
           ; z' <- readIORef z 
           ; writeIORef x (y' + z') }

    foo :: IO () 
    foo = do { x <- newIORef 1 
             ; y <- newIORef 2 
             ; z <- newIORef 65 
             ; plus x y z
             ; x' <- readIORef x 
             ; print x' } 

    comma :: Parser Char 
    comma = char ',' 

    decimal :: Parser Exp
    decimal = 
        do { _ <- string "#"
           ; d <- integer
           ; return $  Dec d}
 
 
    register :: Parser Exp 
    register = 
        do { _ <- char 'r' 
           ; i <- integer 
           ; return $ Reg i }
  
    data Exp = Add    Exp Exp Exp
             | Sub    Exp Exp Exp
             | Move   Exp Exp 
             | Dref   Exp Exp
             | Load   Exp Exp 
             | Store  Exp Exp
             | Reg    Int
             | Dec    Int
             deriving (Show)    
    
    semicolon :: Parser Char 
    semicolon = char ';'

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

    anyExpression :: Parser Exp 
    anyExpression = mov <|> add <|> sub <|> str <|> ldr

    parse :: Parser Exp 
    parse = do { exp <- anyExpression
               ; _   <- eol 
               ; return exp }

    parseAll :: String -> Either String [Exp]
    parseAll lines =
        parseAll' lines []
        where parseAll' []    acc = Right acc
              parseAll' lines acc =  
                   case run parse lines of 
                        Failure msg -> Left msg 
                        Success x r -> parseAll' r (acc ++ [x]) 
              

    getAst :: String -> IO (Either String [Exp]) 
    getAst fileName = 
        do { contents <- readFile fileName 
           ; return $ parseAll contents }
                            

    data CPU = CPU { registers :: [IORef Int]}

    initCPU :: IO CPU 
    initCPU = do { r0 <- newIORef 0
                 ; r1 <- newIORef 0
                 ; return $ CPU [r0,r1]}

    test' :: IO ()
    test' = do { cpu <- initCPU 
               ; let r  = registers cpu
               ; let r0 = r !! 0
               ; x <- readIORef r0
               ; writeIORef r0 (x+3)
               ; y <- readIORef r0
               ; print x 
               ; print y}