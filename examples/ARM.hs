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
                 ; return $  Immed d}
    
 
    register :: Parser Exp 
    register = do { _ <- char 'r' 
                  ; i <- integer 
                  ; return $ Reg i }

    data Op = PLUS | MINUS deriving (Show)
    data R = R0 | R1 | R2 | R3 | R4 | R5 deriving (Show)
    data S = DUMP deriving (Show)
    
    data Ast = Math Op R R R
             | Statement S
               deriving (Show)

    data Exp = Add    Exp Exp Exp
             | Sub    Exp Exp Exp
             | Move   Exp Exp 
             | Dref   Exp Exp
             | Load   Exp Exp 
             | Store  Exp Exp
             | Reg    Int
             | Immed  Int
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

--    baz :: DataProcessing -> L.ByteString 
--    baz (DataProcessing a b c d e) = 
--        P.runPut p 
--        where p = do { P.putWord16le (a .|. (b `shiftL` 11)) 
--                     ; P.putWord16le c}

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
             ; arg <- immediate <|> register 
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
             ; arg <- immediate <|> register 
             ; return $ Sub rd rn arg } 

    move :: Parser Exp
    move = do { _   <- string "MOV" 
              ; _   <- spaces 
              ; rd  <- register 
              ; _   <- comma 
              ; _   <- spaces 
              ; rn  <- register <|> immediate 
              ; return $ Move rd rn }            
    
    derefOffset :: Parser Exp 
    derefOffset = do { _      <- char '['
                     ; rn     <- register
                     ; _      <- comma 
                     ; _      <- spaces
                     ; offset <- immediate 
                     ; _      <- char ']'
                     ; return $ Dref rn offset } 

    derefNoOffset :: Parser Exp 
    derefNoOffset = do { _      <- char '['
                       ; rn     <- register  
                       ; _      <- char ']'
                       ; return $ Dref rn (Immed 0) } 

    deref :: Parser Exp 
    deref = derefOffset <|> derefNoOffset  
    
    load :: Parser Exp
    load = do { _  <- string "LDR"
              ; _  <- spaces
              ; rd <- register
              ; _  <- comma 
              ; _  <- spaces 
              ; d  <- deref 
              ; return $ Load rd d }

    store :: Parser Exp
    store = do { _  <- string "STR"
             ; _  <- spaces
             ; rn <- (label "expect register" register)
             ; _  <- comma 
             ; _  <- spaces 
             ; d  <- deref 
             ; return $ Store rn d }

    dump :: Parser Exp
    dump = (string ":dump") >>. (return $ Reg 0)

    anyExpression :: Parser Exp 
    anyExpression = move <|> add <|> sub <|> store <|> load <|> register

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
--    eval :: Exp -> CPU -> CPU

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
    eval' (Move (Reg d) (Immed x)) cpu = 
        setReg d x cpu 
    eval' (Move (Reg d) (Reg m)) cpu =
        let xm  = getReg m cpu
        in  setReg d xm cpu           

    main :: IO () 
    main = do loop initCPU 
