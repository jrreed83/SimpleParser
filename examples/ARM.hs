module ARM 

where

    import Data.SimpleParser 
    import Text.Printf 
    import qualified Data.ByteString.Lazy as L
    import qualified Data.Word as W
    import qualified Data.Binary.Put as P
    import Data.Bits 

    data Instruction = Instruction { address::Int }


    comma :: Parser Char 
    comma = char ',' 

    immediate :: Parser Exp
    immediate = do { _ <- string "#"
                 ; d <- integer
                 ; return $  Immed (asWord32 d)}
    
 
    register :: Parser Exp 
    register = do { _ <- char 'r' 
                  ; i <- integer 
                  ; return $ Reg (asWord8 i) }

    data Op = ADD | SUB | MOVE | STORE | LOAD | DEREF deriving (Show)

    data Exp = TrinaryOp Op Exp Exp Exp
             | BinaryOp  Op Exp Exp 
             | UnaryOp   Op Exp
             | Reg       W.Word8
             | Immed     W.Word32
             deriving (Show)    

--    data DataProcessing = DataProcessing  { cond :: ! W.Word8 
--                                          , op1  :: ! W.Word8
--                                          , rn   :: ! W.Word8
--                                          , rs   :: ! W.Word8 
--                                          , op2  :: ! W.Word8}

    asWord16 :: (Integral a) => a -> W.Word16 
    asWord16 x = fromIntegral x

    asWord8 :: (Integral a) => a -> W.Word8 
    asWord8 x = fromIntegral x

    asWord32 :: (Integral a) => a -> W.Word32 
    asWord32 x = fromIntegral x

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
             ; rm  <- register 
             ; return $ TrinaryOp ADD rd rn rm } 

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
             ; return $ TrinaryOp SUB rd rn rm } 

    move :: Parser Exp
    move = do { _   <- string "MOV" 
              ; _   <- spaces 
              ; rd  <- register 
              ; _   <- comma 
              ; _   <- spaces 
              ; rn  <- register <|> immediate 
              ; return $ BinaryOp MOVE rd rn }            
    
    derefOffset :: Parser Exp 
    derefOffset = do { _      <- char '['
                     ; rn     <- register
                     ; _      <- comma 
                     ; _      <- spaces
                     ; offset <- immediate 
                     ; _      <- char ']'
                     ; return $ BinaryOp DEREF rn offset } 

    derefNoOffset :: Parser Exp 
    derefNoOffset = do { _      <- char '['
                       ; rn     <- register  
                       ; _      <- char ']'
                       ; return $ BinaryOp DEREF rn (Immed 0) } 

    deref :: Parser Exp 
    deref = derefOffset <|> derefNoOffset  
    
    load :: Parser Exp
    load = do { _  <- string "LDR"
              ; _  <- spaces
              ; rd <- register
              ; _  <- comma 
              ; _  <- spaces 
              ; d  <- deref 
              ; return $ BinaryOp LOAD rd d }

    store :: Parser Exp
    store = do { _  <- string "STR"
             ; _  <- spaces
             ; rn <- (label "expect register" register)
             ; _  <- comma 
             ; _  <- spaces 
             ; d  <- deref 
             ; return $ BinaryOp STORE rn d }

    dump :: Parser Exp
    dump = (string ":dump") >>. (return $ Reg 0)


    anyExpression :: Parser Exp 
    anyExpression = do { _   <- spaces
               ; exp <- move <|> add <|> sub <|> store <|> load <|> register <|> immediate
               ; _   <- eol 
               ; return exp }
    
    parse :: String -> Either String Exp 
    parse str = case run anyExpression str of 
                     Failure msg -> Left msg 
                     Success e _ -> Right e  

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
