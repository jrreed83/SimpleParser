module ARM 

where

    import Data.SimpleParser 
    import Text.Printf 
    import qualified Data.ByteString.Lazy as L
    import qualified Data.Word as W
    import qualified Data.Binary.Put as P
    import Data.Bits 
    import Data.IORef 

    data Instruction = Instruction { address::Int }


    comma :: Parser Char 
    comma = char ',' 

    semicolon :: Parser Char 
    semicolon = char ';' 

    lbracket :: Parser Char 
    lbracket = char '['

    rbracket :: Parser Char 
    rbracket = char ']'

    comments :: Parser String 
    comments = semicolon >> anyString

    immediate :: Parser Immed
    immediate = do { _ <- string "#"
                   ; d <- integer
                   ; return $ Immed (asWord32 d)}

    register :: Parser Reg 
    register = do { _ <- char 'r' 
                  ; i <- integer 
                  ; return $ Reg (asWord8 i) }

    data Reg = Reg W.Word32 deriving Show 
    
    data Immed = Immed W.Word32 deriving Show 

    data Op = ADD | SUB | MOVE | STORE | LOAD | DEREF deriving (Show)

    data Exp = Exp3 Op Reg Reg Reg
             | Exp2 Op Reg Reg 
             deriving (Show)    


    encodeOp :: Op -> W.Word32 
    encodeOp ADD   = 1 
    encodeOp SUB   = 2 
    encodeOp MOVE  = 3
    encodeOp STORE = 4
    encodeOp LOAD  = 5 
    encodeOp DEREF = 6

    decodeOp :: W.Word32 -> Op 
    decodeOp 1 = ADD 
    decodeOp 2 = SUB 
    decodeOp 3 = MOVE 
    decodeOp 4 = STORE 
    decodeOp 5 = LOAD 
    decodeOp 6 = DEREF 


    asWord16 :: (Integral a) => a -> W.Word16 
    asWord16 x = fromIntegral x

    asWord8 :: (Integral a) => a -> W.Word8 
    asWord8 x = fromIntegral x

    asWord32 :: (Integral a) => a -> W.Word32 
    asWord32 x = fromIntegral x

    asInt :: (Integral a) => a -> Int 
    asInt x = fromIntegral x 

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
             ; rm  <- register 
             ; return $ Exp3 ADD rd rn rm } 

    sub :: Parser Exp
    sub = do { _   <- string "SUB" 
             ; _   <- spaces 
             ; rd  <- register 
             ; _   <- comma 
             ; _   <- spaces 
             ; rn  <- register 
             ; _   <- comma 
             ; _   <- spaces 
             ; rm  <- register 
             ; return $ Exp3 SUB rd rn rm } 

--    move :: Parser Exp
--    move = do { _   <- string "MOV" 
--              ; _   <- spaces 
--              ; rd  <- register 
--              ; _   <- comma 
--              ; _   <- spaces 
--              ; rn  <- register <|> immediate 
--              ; return $ Exp2 MOVE rd rn }            
    
--    derefOffset :: Parser Exp 
--    derefOffset = do { _      <- lbracket
--                     ; rn     <- register
--                     ; _      <- comma 
--                     ; _      <- spaces
--                     ; offset <- immediate 
--                     ; _      <- rbracket
--                     ; return $ Exp2 DEREF rn offset } 

--    derefNoOffset :: Parser Exp 
--    derefNoOffset = do { _      <- lbracket
--                       ; rn     <- register  
--                       ; _      <- rbracket
--                       ; return $ Exp2 DEREF rn (Num 0) } 

--    deref :: Parser Exp 
--    deref = derefOffset <|> derefNoOffset  
    
--    load :: Parser Exp
--    load = do { _  <- string "LDR"
--              ; _  <- spaces
--              ; rd <- register
--              ; _  <- comma 
--              ; _  <- spaces 
--              ; d  <- deref 
--              ; return $ Exp2 LOAD rd d }

--    store :: Parser Exp
--    store = do { _  <- string "STR"
--             ; _  <- spaces
--             ; rn <- (label "expect register" register)
--             ; _  <- comma 
--             ; _  <- spaces 
--             ; d  <- deref 
--             ; return $ Exp2 STORE rn d }

    --dump :: Parser Exp
    --dump = (string ":dump") >>. (return $ Reg 0)

    anyExpression :: Parser Exp 
    anyExpression = do { _   <- spaces
                       ; exp <- add <|> sub 
                       ; _   <- eol 
                       ; return exp }
    
    parse :: String -> Either String Exp 
    parse str = case run anyExpression str of 
                     Failure msg -> Left msg 
                     Success e _ -> Right e  

    data CPU = CPU { genReg :: [IORef W.Word32]
                   , pc     :: IORef W.Word32
                   , sp     :: IORef W.Word32 
                   , lr     :: IORef W.Word32}

    initCPU :: IO CPU 
    initCPU = do { r0  <- newIORef 0
                 ; r1  <- newIORef 0
                 ; r2  <- newIORef 0  
                 ; r3  <- newIORef 0
                 ; r4  <- newIORef 0 
                 ; r5  <- newIORef 4
                 ; r6  <- newIORef 6  
                 ; r7  <- newIORef 0
                 ; r8  <- newIORef 0    
                 ; r9  <- newIORef 0
                 ; r10 <- newIORef 0  
                 ; r11 <- newIORef 0
                 ; r12 <- newIORef 0        
                 ; pc  <- newIORef 0 
                 ; sp  <- newIORef 0 
                 ; lr  <- newIORef 0
                 ; return $ CPU [r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12] pc sp lr} 
               
    eval :: Exp -> CPU -> IO W.Word32 
    eval (Exp3 ADD (Reg d) (Reg m) (Reg n)) cpu = 
        do { let r  = genReg cpu
           ; let m' = (asInt m)
           ; let n' = (asInt n)   
           ; let d' = (asInt d)                         
           ; rm <- readIORef (r !! m') 
           ; rn <- readIORef (r !! n')
           ; _  <- writeIORef (r !! d') (rm + rn)
           ; rd <- readIORef (r !! d')
           ; return rd 
        }

    foo :: IO () 
    foo = do { cpu <- initCPU 
             ; x   <- eval (Exp3 ADD (Reg 4) (Reg 5) (Reg 6)) cpu 
             ; print x }

--    loop :: CPU -> IO () 
--    loop cpu = do   
--                 putStr ">>"
--                 l <- getLine -- read 
                 -- Want to check whether it's an expression
                 -- or some sort of debug command
--                 case eval l cpu of 
--                    Right cpu' -> loop cpu'
--                    Left  msg  -> loop cpu  
    
       

--    main :: IO () 
--    main = do loop initCPU 
