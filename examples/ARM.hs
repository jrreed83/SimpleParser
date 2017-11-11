module ARM 

where

    import Data.SimpleParser 
    import Text.Printf 
    import qualified Data.ByteString as B

    data Instruction = Instruction { address::Int }

    comma :: Parser Char 
    comma = char ',' 

    decimal :: Parser Exp
    decimal = do { _ <- string "#"
                 ; d <- integer
                 ; return $  Dec d}
    
 
    register :: Parser Exp 
    register = do { _ <- char 'r' 
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
    add = do { _   <- string "ADD" 
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
    sub = do { _   <- string "SUB" 
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
    mov = do { _   <- string "MOV" 
             ; _   <- spaces 
             ; rd  <- register 
             ; _   <- comma 
             ; _   <- spaces 
             ; rn  <- register <|> decimal 
             ; return $ Move rd rn }            
    
    derefOffset :: Parser Exp 
    derefOffset = do { _      <- char '['
                     ; rn     <- register
                     ; _      <- comma 
                     ; _      <- spaces
                     ; offset <- decimal 
                     ; _      <- char ']'
                     ; return $ Dref rn offset } 

    derefNoOffset :: Parser Exp 
    derefNoOffset = do { _      <- char '['
                       ; rn     <- register  
                       ; _      <- char ']'
                       ; return $ Dref rn (Dec 0) } 

    deref :: Parser Exp 
    deref = derefOffset <|> derefNoOffset  
    
    ldr :: Parser Exp
    ldr = do { _  <- string "LDR"
             ; _  <- spaces
             ; rd <- register
             ; _  <- comma 
             ; _  <- spaces 
             ; d  <- deref 
             ; return $ Load rd d }

    str :: Parser Exp
    str = do { _  <- string "STR"
             ; _  <- spaces
             ; rn <- (label "expect register" register)
             ; _  <- comma 
             ; _  <- spaces 
             ; d  <- deref 
             ; return $ Store rn d }

--    dump :: Parser Debug
--    dump = (string ":dump") >>= return Dump

    anyExpression :: Parser Exp 
    anyExpression = mov <|> add <|> sub <|> str <|> ldr <|> register

    parse :: Parser Exp 
    parse = do { _   <- spaces
               ; exp <- anyExpression
               ; _   <- eol 
               ; return exp }
                            
    data CPU = CPU { r0 :: Int 
                   , r1 :: Int
                   , r2 :: Int 
                   , r3 :: Int 
                   , r4 :: Int 
                   , r5 :: Int}

    initCPU :: CPU 
    initCPU = CPU 0 0 0 0 0 0
               
    getReg :: Int -> CPU -> Int 
    getReg 0 (CPU a b c d e f) = a 
    getReg 1 (CPU a b c d e f) = b  
    getReg 2 (CPU a b c d e f) = c 
    getReg 3 (CPU a b c d e f) = d  
    getReg 4 (CPU a b c d e f) = e  

    setReg :: Int -> Int -> CPU -> CPU 
    setReg 0 x (CPU a b c d e f) = CPU x b c d e f
    setReg 1 x (CPU a b c d e f) = CPU a x c d e f  
    setReg 2 x (CPU a b c d e f) = CPU a b x d e f
    setReg 3 x (CPU a b c d e f) = CPU a b c x e f      
    setReg 4 x (CPU a b c d e f) = CPU a b c d x f  
    setReg 5 x (CPU a b c d e f) = CPU a b c d e x  

    loop :: CPU -> IO () 
    loop cpu = do   
                 putStr ">>"
                 l <- getLine -- read 
                 -- Want to check whether it's an expression
                 -- or some sort of debug command
                 case eval l cpu of 
                    Right cpu' -> loop cpu'
                    Left  msg  -> loop cpu  
    
    compileToExp :: String -> Either String Exp 
    compileToExp str = case run parse str of
                            Success exp _ -> Right exp 
                            Failure msg   -> Left msg 

--    compileToByteCode :: Exp -> 

    eval :: String -> CPU -> Either String CPU
    eval str cpu = 
        case run parse str of
            Success exp _ -> Right $ eval' exp cpu 
            Failure msg   -> Left msg
        
    eval' :: Exp -> CPU -> CPU
    eval' (Add (Reg d) (Reg n) (Reg m)) cpu = 
        let xn = getReg n cpu
            xm = getReg m cpu
        in  setReg d (xm + xn) cpu
    eval' (Move (Reg d) (Dec x)) cpu = 
        setReg d x cpu 
    eval' (Move (Reg d) (Reg m)) cpu =
        let xm  = getReg m cpu
        in  setReg d xm cpu           

    main :: IO () 
    main = do loop initCPU 
