module SimpleParser where

    data Result = Success 
                | Error String
                deriving (Show, Eq)

    data Parser = Parser {run :: String -> Result}

    pchar :: Char -> Parser
    pchar c =
        Parser $
             \s -> if (head s) == c then Success else Error ("Expected " ++ [c] ++ " but got " ++ [head s])          
             
--    andThen :: Parser -> Parser -> Parser
--    andThen parser1 parser2 = 
--        Parser $
--             \s -> case run parser1 s of 
--                        Error s  -> Error s 
--                        Success  -> 