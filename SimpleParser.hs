module SimpleParser where

    data Result a = Success {match :: a, count :: Int}
                  | Failure {msg :: String}
                  deriving (Show, Eq)

    data Parser a = Parser {run :: String -> Result a}

--    pChar :: Char -> Parser Char
--    pChar c =
--        Parser $
--             \s -> 
--                if (head s) == c then 
--                    Success c 1  
--                else 
--                    Failure ("Expected " ++ [c] ++ " but got " ++ [head s])          
             
    pChar :: Char -> Parser Char
    pChar x = Parser fn 
              where fn (h:t) | (h == x)  = Success x 1 
                             | otherwise = Failure ("Expected " ++ [x] ++ " but got " ++ [h])  

    pDigit :: Int -> Parser Int
    pDigit x = Parser fn
               where fn (h:t) | (h == ((head . show) x)) = Success x 1 
                              | otherwise                = Failure ("Error")  

    andThen :: Parser a -> Parser b -> Parser (a,b)
    andThen parser1 parser2 = 
        Parser fn 
        where fn s = case run parser1 s of 
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
