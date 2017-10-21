module SimpleParser where

    data Result = Success {match::String, rest::String}
                | Failure {msg::String}
                deriving (Show, Eq)

    data Parser = Parser {run :: String -> Result}

    pchar :: Char -> Parser
    pchar c =
        Parser $
             \s -> 
                if (head s) == c then 
                    Success [c] (tail s)  
                else 
                    Failure ("Expected " ++ [c] ++ " but got " ++ [head s])          
             
    andThen :: Parser -> Parser -> Parser
    andThen parser1 parser2 = 
        Parser $
             \s -> case run parser1 s of 
                        Failure x      -> Failure x 
                        Success m1 r1  -> case run parser2 r1 of
                                               Failure x      -> Failure x 
                                               Success m2 r2  -> Success (m1++m2) r2   

    (.>>.) :: Parser -> Parser -> Parser 
    (.>>.) = andThen

--    alt :: Parser -> Parser -> Parser 
--    alt parser1 parser2 = 
--        Parser $
--             \s -> case run parser1 s of
--                        Success   -> Success
--                        Failure _ -> case run parser2 s of 
--                                          Failure _ -> Failure "Couldn't match anything"
--                                          Success   -> Success 

--    (<|>) :: Parser -> Parser -> Parser 
--    (<|>) = alt