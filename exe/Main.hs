module Main where
import Parser
import MathExpr
import Text.Parsec (ParseError)

main :: IO ()
main = putStrLn "Hello, Haskell!"



simpleEvaluator :: String -> Either ParseError MathExpr
simpleEvaluator s = eval <$> doParse s

symbolicTest s = do
    let res = simpleEvaluator s 
    print res
    print $ symbolicStep <$> res
    -- symbolicStep <$> simpleEvaluator s  

-- >>> 