module SimpleParser where

    data Result a = Success {match :: a, rest :: String}
                  | Failure {msg :: String}
                  deriving (Show, Eq)

    data Parser a = Parser {run :: String -> Result a}

    instance Functor Parser where
        fmap f pa = Parser $ 
                        \s -> case run pa s of 
                                   Failure m   -> Failure m 
                                   Success x c -> Success (f x) c        
             
    char :: Char -> Parser Char
    char x = Parser $  
                   \(h:t) -> if (h == x) then 
                                Success x t 
                             else 
                                Failure ("Expected " ++ [x] ++ " but got " ++ [h])  

    unit :: a -> Parser a 
    unit x = Parser ( \s -> Success x s )

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
        Parser $ 
             \s -> case run parser1 s of 
                        Failure x      -> Failure x 
                        Success m1 r1  -> case run parser2 r1 of
                                               Failure m      -> Failure m 
                                               Success m2 r2  -> Success (m1,m2) r2   

    (.>>.) :: Parser a -> Parser b -> Parser (a,b) 
    (.>>.) = andThen

    alt :: Parser a -> Parser a -> Parser a 
    alt parser1 parser2 = 
        Parser $
             \s -> case run parser1 s of
                        Success m r -> Success m r
                        Failure _   -> case run parser2 s of 
                                            Failure m   -> Failure "Couldn't match anything"
                                            Success m r -> Success m r 

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
    map2 f pa pb = fmap f (pa .>>. pb)

    many :: Parser a -> Parser [a]
    many pa = 
        Parser (fn [])
        where fn accum [] = Success accum [] 
              fn accum s = case run pa s of
                Failure _    -> Success accum s
                Success x r  -> fn (accum ++ [x]) r
 
    (>>.) :: Parser a -> Parser b -> Parser b 
    pa >>. pb = fmap (\(a,b) -> b) (pa .>>. pb)

    (.>>) :: Parser a -> Parser b -> Parser a 
    pa .>> pb = fmap (\(a,b) -> a) (pa .>>. pb)

    data Date = Date { day :: Int, month :: Int, year :: Int} 
                deriving (Show)

    date :: Parser Date
    date = fmap (\(x,(y,z)) -> Date x y z) (integer .>>. ((char '/') >>. integer .>>. ((char '/') >>. integer)))