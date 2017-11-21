module Motorolla86000

where

    import Data.SimpleParser 
    import qualified Data.ByteString as B 
    import qualified Data.Word as W
    import Data.Bits 
    import Data.IORef 
    import qualified Data.Vector as V 
    -----------------------------------------------------------
    --  Some additional parsing function 
    -----------------------------------------------------------
    eol :: Parser String 
    eol = (string "\n") <|> spaces

    comma :: Parser Char 
    comma = char ',' 

    semicolon :: Parser Char 
    semicolon = char ';' 


    lparen :: Parser Char 
    lparen = char '(' 

    rparen :: Parser Char 
    rparen = char ')'

    comments :: Parser String 
    comments = semicolon >> anyString

    u8 :: Parser W.Word8 
    u8 = integer >>= (\x -> return $ asWord8 x)

    u16 :: Parser W.Word16 
    u16 = integer >>= (\x -> return $ asWord16 x)

    u32 :: Parser W.Word32 
    u32 = integer >>= (\x -> return $ asWord32 x)

    data AddressMode = Data         W.Word8
                     | AddrD        W.Word8 
                     | AddrI        W.Word8
                     | AddrPostInc  W.Word8
                     | AddrPostDec  W.Word8 
                     | AddrPreInc   W.Word8
                     | AddrPreDec   W.Word8
                     | Immed        W.Word32  
                     deriving (Show)

    encAddMode :: AddressMode -> [W.Word8] 
    encAddMode (Data  i)       = [0, i] 
    encAddMode (AddrD i)       = [1, i]  -- 0001,i
    encAddMode (AddrI i)       = [2, i]  -- 0010,i 
    encAddMode (AddrPostInc i) = [3, i]
    encAddMode (AddrPostDec i) = [4, i]
    encAddMode (AddrPreInc i)  = [5, i]
    encAddMode (AddrPreDec i)  = [6, i]
    encAddMode (Immed i)       = [7, i0, i1, i2, i3] 
                                 where i0 = asWord8 $ (shiftR i 0 ) .&. 0xff 
                                       i1 = asWord8 $ (shiftR i 8 ) .&. 0xff 
                                       i2 = asWord8 $ (shiftR i 16) .&. 0xff 
                                       i3 = asWord8 $ (shiftR i 24) .&. 0xff                                        
    pData :: Parser AddressMode 
    pData = do { _ <- char 'd' 
               ; i <- u8 
               ; return $ Data i }

    pAddrD :: Parser AddressMode 
    pAddrD = do { _ <- char 'a' 
                ; i <- u8 
                ; return $ AddrD i }

    pAddrI :: Parser AddressMode
    pAddrI = do { _ <- lparen 
                ; _ <- char 'a' 
                ; i <- u8;
                ; _ <- rparen
                ; return $ AddrI i }
    

    isDataReg :: AddressMode -> Bool 
    isDataReg (Data _) = True 
    isDataReg _        = False

    pAddrPostInc :: Parser AddressMode 
    pAddrPostInc = (pAddrI .>> (char '+')) >>= (\(AddrI x) -> return $ AddrPostInc x)

    pAddrPostDec :: Parser AddressMode 
    pAddrPostDec = (pAddrI .>> (char '-')) >>= (\(AddrI x) -> return $ AddrPostDec x)

    pAddrPreInc :: Parser AddressMode 
    pAddrPreInc = ((char '+') >>. pAddrI) >>= (\(AddrI x) -> return $ AddrPreInc x)

    pAddrPreDec :: Parser AddressMode 
    pAddrPreDec = ((char '-') >>. pAddrI) >>= (\(AddrI x) -> return $ AddrPreDec x)

    pImmed :: Parser AddressMode 
    pImmed = do { _ <- char '#' 
                ; i <- u32 
                ; return $ Immed i}

    pAddressMode = pAddrPreDec <|> pAddrPreInc <|> pAddrPostInc <|> pAddrPostDec <|> pAddrI <|> pAddrD <|> pData <|> pImmed
    
    data OpMode = Byte | Word | Long deriving (Show)

    encOpMode :: OpMode -> W.Word8 
    encOpMode Byte = 0 
    encOpMode Word = 1 
    encOpMode Long = 2 

    data Instruction = MOVE OpMode AddressMode AddressMode
                     | ADD  OpMode AddressMode AddressMode
                     | NOP
                     deriving (Show)
 
    pByte :: Parser OpMode 
    pByte = (string ".b") >>= (\_ -> return Byte) 

    pWord :: Parser OpMode 
    pWord = (string ".w") >>= (\_ -> return Word) 
    
    pLong :: Parser OpMode 
    pLong = (string ".l") >>= (\_ -> return Long) 

    pOpMode :: Parser OpMode 
    pOpMode = pByte <|> pLong <|> pWord 

    asWord16 :: (Integral a) => a -> W.Word16 
    asWord16 x = fromIntegral x

    asWord8 :: (Integral a) => a -> W.Word8 
    asWord8 x = fromIntegral x

    asWord32 :: (Integral a) => a -> W.Word32 
    asWord32 x = fromIntegral x

    asInt :: (Integral a) => a -> Int 
    asInt x = fromIntegral x 

    -- Instruction set

    nop :: Parser Instruction 
    nop = (string "nop") >>= (\_ -> return NOP)

    move :: Parser Instruction
    move = do { _    <- string "move"
              ; op   <- pOpMode    
              ; _    <- spaces
              ; src  <- pAddressMode
              ; _    <- comma 
              ; dst  <- pAddressMode
              ; return $ MOVE op src dst}    

    add :: Parser Instruction
    add = do { _    <- string "add"
             ; op   <- pOpMode    
             ; _    <- spaces
             ; src  <- pAddressMode
             ; _    <- comma 
             ; dst  <- pAddressMode
             ; if isDataReg (src) || isDataReg (dst) then return (ADD op src dst) else failure "uh oh"
             }   

    encode :: Instruction -> [W.Word8]
    encode NOP               = [0] 
    encode (MOVE op src dst) = [1, encOpMode op] ++ encAddMode src ++ encAddMode dst 
    encode (ADD  op src dst) = [2, encOpMode op] ++ encAddMode src ++ encAddMode dst 

    type Memory = V.Vector W.Word8 
    
    initMemory :: W.Word32 -> Memory 
    initMemory n = V.fromList [1,1,2,255]--V.replicate (asInt n) (0::W.Word8)

    memoryLookup :: (Integral a) => a -> Memory -> W.Word8 
    memoryLookup i mem = mem V.! (asInt i)

    data CPU = CPU { ip     :: IORef W.Word32
                   , sp     :: IORef W.Word32 
                   , memory :: Memory}

    initCPU :: IO CPU 
    initCPU =  do { ip <- newIORef 0;
                  ; sp <- newIORef 0;
                  ; return $ CPU ip sp (initMemory 256)}

    nextInst :: CPU -> IO W.Word8
    nextInst cpu = let ip'  = ip cpu
                       mem' = memory cpu
                   in  do { i <- readIORef ip' 
                          ; modifyIORef ip' (+ 1)
                          ; return $ memoryLookup i mem'}

    execute :: IO () 
    execute = 
        do  
           cpu <- initCPU 
           execute' cpu

    execute' :: CPU -> IO ()
    execute cpu = 
        do  
           x <- nextInst cpu 
           case x of 
                255 -> do { putStrLn "Done"
                          ; return ()}
                _   -> do { putStrLn (show x)
                          ; execute cpu}     
    
--    initCPU :: IO CPU 
--    initCPU = do { r0  <- newIORef 0
--                 ; r1  <- newIORef 0
--                 ; r2  <- newIORef 0  
--                 ; r3  <- newIORef 0
--                 ; r4  <- newIORef 0 
--                 ; r5  <- newIORef 4
--                 ; r6  <- newIORef 6  
--                 ; r7  <- newIORef 0
--                 ; r8  <- newIORef 0    
--                 ; r9  <- newIORef 0
--                 ; r10 <- newIORef 0  
--                 ; r11 <- newIORef 0
--                 ; r12 <- newIORef 0        
--                 ; pc  <- newIORef 0 
--                 ; sp  <- newIORef 0 
--                 ; lr  <- newIORef 0
--                 ; return $ CPU [r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12] pc sp lr} 
               
--    eval :: Exp -> CPU -> IO W.Word32 
--    eval (Exp3 ADD (Reg d) (Reg m) (Reg n)) cpu = 
 --       do { let r  = genReg cpu
--           ; let m' = (asInt m)
--           ; let n' = (asInt n)   
--           ; let d' = (asInt d)                         
--           ; rm <- readIORef (r !! m') 
--           ; rn <- readIORef (r !! n')
--           ; _  <- writeIORef (r !! d') (rm + rn)
--           ; rd <- readIORef (r !! d')
--           ; return rd 
--        }

--    foo :: IO () 
--    foo = do { cpu <- initCPU 
--             ; x   <- eval (Exp3 ADD (Reg 4) (Reg 5) (Reg 6)) cpu 
--             ; print x }

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
