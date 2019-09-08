{-
    Making a Ternary Operator in Haskell
    Andrew Ribeiro
    September 2019
-}

module TernaryOperator where
import Data.Char (toUpper)

(?) :: Bool -> a -> Maybe a
True  ? x = Just x
False ? _ = Nothing 
infixl 3 ?

(<|>) :: Maybe a -> a -> a
Nothing <|> x = x 
Just x  <|> _ = x
infixl 3 <|>

askQuestion :: String -> [String] -> IO (Maybe String)
askQuestion str validAnswers = do
    putStrLn $ "> "++str++"   "++show validAnswers
    answer <- getLine
    let upperAns = toUpper <$> answer
    upperAns `elem` validAnswers ? 
        return (Just upperAns) <|> return Nothing
        
main = do
    answer <- askQuestion "Was it good service?" ["YES","NO"]
    case answer of
        Just a  -> putStrLn $ "< Tip " ++ (a == "YES" ? "0.25%" <|> "0.10%")
        Nothing -> putStrLn "Error: Invalid input."

-- >>> True ? "first value" <|> "second value"
-- "first value"
--
-- >>> False ? "first value" <|> "second value"
-- "second value"
--
