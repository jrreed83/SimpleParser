module SimpleParser where

    data Result a = Success {match :: a, count :: Int}
                  | Failure {msg :: String}
                  deriving (Show, Eq)

    data Parser a = Parser {run :: String -> Result a}

    pChar :: Char -> Parser Char
    pChar c =
        Parser $
             \s -> 
                if (head s) == c then 
                    Success c 1  
                else 
                    Failure ("Expected " ++ [c] ++ " but got " ++ [head s])          
             
    andThen :: Parser a -> Parser b -> Parser (a,b)
    andThen parser1 parser2 = 
        Parser $
             \s -> case run parser1 s of 
                        Failure x      -> Failure x 
                        Success m1 c1  -> case run parser2 (drop c1 s) of
                                               Failure x      -> Failure x 
                                               Success m2 c2  -> Success (m1,m2) (c1+c2)   

    (.>>.) :: Parser a -> Parser b -> Parser (a,b) 
    (.>>.) = andThen

    alt :: Parser a -> Parser a -> Parser a 
    alt parser1 parser2 = 
        Parser $
             \s -> case run parser1 s of
                        Success m c -> Success m c
                        Failure _   -> case run parser2 s of 
                                          Failure _   -> Failure "Couldn't match anything"
                                          Success m c -> Success m c 

    (<|>) :: Parser a -> Parser a -> Parser a 
    (<|>) = alt

    pDigit :: Parser Char
    pDigit =  (pChar '0') <|> (pChar '1') <|> (pChar '2') <|> (pChar '3') <|> (pChar '4') 
          <|> (pChar '5') <|> (pChar '6') <|> (pChar '7') <|> (pChar '8') <|> (pChar '9') 