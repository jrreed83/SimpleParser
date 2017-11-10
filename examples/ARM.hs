module ARM 

where

    import Data.SimpleParser
    import Data.IORef 
    import Text.Printf 

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

    anyExpression :: Parser Exp 
    anyExpression = mov <|> add <|> sub <|> str <|> ldr <|> register

    parse :: Parser Exp 
    parse = do { _   <- spaces
               ; exp <- anyExpression
               ; _   <- eol 
               ; return exp }
                            

    data CPU = CPU {registers    :: [IORef Int]}

    initCPU :: IO CPU 
    initCPU = do { r0 <- newIORef 0
                 ; r1 <- newIORef 0
                 ; r2 <- newIORef 0     
                 ; r3 <- newIORef 0
                 ; r4 <- newIORef 0   
                 ; r5 <- newIORef 0                                               
                 ; return $ CPU [r0,r1,r2,r3,r4,r5]}
                   

    loop :: CPU -> IO () 
    loop cpu = do   
                 putStr ">>"
                 l <- getLine -- read 
                 eval l cpu   -- eval
                 --dump cpu     -- print
                 loop cpu     -- loop
    
    eval :: String -> CPU -> IO ()
    eval str cpu = 
        case run parse str of
            Success exp _ -> eval' exp cpu 
            Failure msg   -> putStrLn "ok"
        
    eval' :: Exp -> CPU -> IO () 
    eval' (Add (Reg d) (Reg n) (Reg m)) cpu = 
        do { let r = registers cpu 
           ; x <- readIORef (r !! n)
           ; y <- readIORef (r !! m)
           ; writeIORef (r !! d) (x+y) }
    eval' (Move (Reg d) (Dec x)) cpu = 
        do { let r = registers cpu 
           ; writeIORef (r !! d) x }
    eval' (Move (Reg d) (Reg m)) cpu = 
        do { let r = registers cpu
           ; x <- readIORef ( r !! m) 
           ; writeIORef (r !! d) x }  
    eval' (Reg d) cpu = 
        let r = (registers cpu) !! d 
        in (readIORef r) >>= (\x -> putStrLn $ show x)

    dump :: CPU -> IO () 
    dump cpu = do { let r  = registers cpu 
                  ; x0 <- readIORef $ r !! 0
                  ; x1 <- readIORef $ r !! 1
                  ; x2 <- readIORef $ r !! 2
                  ; x3 <- readIORef $ r !! 3
                  ; x4 <- readIORef $ r !! 4
                  ; x5 <- readIORef $ r !! 5
                  ; putStrLn $ printf "0x%04X 0x%04X" x0 x1 }

    main :: IO () 
    main = do cpu <- initCPU 
              loop cpu 
