module TernaryOperator where

class TernaryOP a where
    (?) :: Bool -> a -> a
    (<|>) :: a -> a -> a

instance TernaryOP (Maybe a) where
    True  ? (Just fn) = Just fn
    False ? _       = Nothing 

    Nothing <|> x@(Just fn) = x 
    x@(Just fn) <|> _ = x

(-->) :: Maybe a -> (a->b) -> b
Just a --> fn = fn a
    
-- >>> False ? Just 3 <|> Just 4 --> (+10)
-- 14
--
-- >>> if False then 3 else 4 + 10
-- 14
--

-- >>> Nothing <|> Just 2  <|>  Nothing  <|> Just 1 
-- Just 2
--

askQuestion :: String -> IO String
askQuestion str = do
    putStrLn str
    getLine

areYouGood = do
    answer <- askQuestion "Are you good?"
    (answer == "YES") ? (Just "Good boy!") <|> (Just "Bad boy!") --> putStrLn 

-- >>> areYouGood