module Data.SimpleParser 
    (
      Parser(..)
    , Result(..)
    , char
    , success
    , string
    , digit
    , anyDigit
    , integer
    , (.>>.)
    , (<|>)
    , (.>>)
    , (>>.)
    , many
    , many1
    , exactlyN
    , spaces
    ) where
        
    import Data.Time
    import Text.Printf

    data Result a = Success {match :: a, rest :: String}
                  | Failure {msg :: String}
                  deriving (Show, Eq)

    data Parser a = Parser { run :: String -> Result a }

    parse :: Parser a -> String -> Result a 
    parse pa str = case run pa str of 
                        Failure msg  -> Failure msg 
                        Success x "" -> Success x ""
                        Success x r  -> Failure "Failed to parse entire"

    instance Functor Parser where
        fmap f pa = Parser $ 
                        \s -> case run pa s of 
                                   Failure m   -> Failure m 
                                   Success x c -> Success (f x) c        

    instance Applicative Parser where 
        pure x    = success x 
        pf <*> pa = apply pf pa


    instance Monad Parser where 
        pa >>= f = bind pa f 
        pa >> pb = pa >>. pb
        return x = success x 

    char :: Char -> Parser Char
    char x = Parser $  
                   \(h:t) -> if (h == x) then 
                                Success x t 
                             else 
                                Failure ("Expected " ++ [x] ++ " but got " ++ [h])  

    success :: a -> Parser a 
    success x = Parser ( \s -> Success x s )

    string :: String -> Parser String
    string x = Parser $
                     \s -> let n = length x
                           in  if (x == (take n s)) then 
                                    Success x (drop n s)
                               else 
                                    Failure "Error"

    digit :: Int -> Parser Int
    digit x = fmap (\c -> (read c :: Int)) (string (show x))

    anyDigit :: Parser Int 
    anyDigit = (digit 0) <|> (digit 1) <|> (digit 2) <|> (digit 3) <|> (digit 4) <|> 
               (digit 5) <|> (digit 6) <|> (digit 7) <|> (digit 8) <|> (digit 9) 

    integer :: Parser Int
    integer = 
        fmap (\l -> (fn l)) (many anyDigit)
        where fn l = read (map (head . show) l) :: Int


    andThen :: Parser a -> Parser b -> Parser (a,b)
    andThen parser1 parser2 = 
         do { x <- parser1
            ; y <- parser2
            ; return (x,y) }
--        Parser $ 
--             \s -> case run parser1 s of 
--                        Failure x      -> Failure x 
--                        Success m1 r1  -> case run parser2 r1 of
--                                               Failure m      -> Failure m 
--                                               Success m2 r2  -> Success (m1,m2) r2   

    (.>>.) :: Parser a -> Parser b -> Parser (a,b) 
    (.>>.) = andThen

    alt :: Parser a -> Parser a -> Parser a 
    alt parser1 parser2 = 
        Parser $
             \s -> case run parser1 s of
                        Success m r -> Success m r
                        Failure _   -> run parser2 s

    (<|>) :: Parser a -> Parser a -> Parser a 
    (<|>) = alt

    slice :: Parser a -> Parser String 
    slice p = 
        Parser $
             \s ->  case run p s of
                         Failure m   -> Failure m
                         Success x r -> let n = (length s) - (length r)
                                        in  Success (take n s) r

    map2 :: ((a,b) -> c) -> Parser a -> Parser b -> Parser c
--    map2 f pa pb = fmap f (pa .>>. pb)
    map2 f pa pb = 
        do { x <- pa 
           ; y <- pb 
           ; return $ f (x, y) }

    many :: Parser a -> Parser [a]
    many pa = 
        Parser (fn [])
        where fn accum [] = Success accum [] 
              fn accum s = case run pa s of
                Failure _    -> Success accum s
                Success x r  -> fn (accum ++ [x]) r
 
    many1 :: Parser a -> Parser [a]
    --many1 pa = fmap (\(x,y) -> x : y) (pa .>>. (many pa))
    many1 pa = do { xa <- pa
                  ; la <- many pa
                  ; return (xa : la)}

    exactlyN :: Parser a -> Int -> Parser [a]
    exactlyN pa n = 
        Parser $ 
             \s -> case run (many pa) s of 
                        Failure msg   -> Failure msg 
                        Success lst r -> if (length lst) == n 
                                         then (Success lst r) 
                                         else Failure "Error"

    {-- --}
    bind :: Parser a -> (a -> Parser b) -> Parser b 
    bind pa f = 
        Parser $
             \s -> case run pa s of
                        Failure msg1  -> Failure msg1
                        Success x1 r1 -> run (f x1) r1 

    apply :: Parser (a -> b) -> Parser a -> Parser b 
    apply pf pa = do { f  <- pf
                     ; xa <- pa 
                     ; return (f xa)}
--        Parser $
--             \s -> case run pf s of
--                        Failure msg -> Failure msg 
--                        Success f _ -> case run pa s of
--                                            Failure msg -> Failure msg 
--                                            Success x r -> Success (f x) r
    

    (>>.) :: Parser a -> Parser b -> Parser b 
    --pa >>. pb = pa >>= (\_ -> pb)
    --pa >>. pb = fmap (\(a,b) -> b) (pa .>>. pb)
    pa >>. pb = do { _  <- pa 
                   ; xb <- pb
                   ; return xb }

    (.>>) :: Parser a -> Parser b -> Parser a 
    --pa .>> pb = fmap (\(a,b) -> a) (pa .>>. pb)
    pa .>> pb = do { xa <- pa 
                   ; _  <- pb
                   ; return xa }

    data Date = Date { month :: Int, day :: Int, year :: Int} 
                deriving (Show)

    translate :: Date -> UTCTime
    translate (Date m d y) = 
        let str = printf "%02d/%02d/%d" m d y
        in  parseTimeOrError False defaultTimeLocale "%m/%d/%Y" str :: UTCTime

    spaces :: Parser String 
    spaces = many (char ' ')

    date :: Parser Date
    date = do { month <- integer 
              ; char '/'
              ; day <- integer
              ; char '/' 
              ; year <- integer
              ; return $ Date month day year }
    --date = fmap (\(x,(y,z)) -> Date x y z) (integer .>>. ((char '/') >>. integer .>>. ((char '/') >>. integer)))

