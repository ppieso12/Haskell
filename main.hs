main :: IO ()

main = do
	putStrLn "Podaj imie "
      name <- getLine
      let msg = "Witaj  " ++ name 
      putStrLn  msg




{-data Maybe a = Just a | Nothing

-- Box = Just
instance Functor Box where
fmap f Empty = Empty
fmap f (Box b) = Box (f b)
     



-}
{-
main :: IO Int
main = do
      putStrLn "Podaj imie "
      name <- getLine
      let msg = "Witaj  " ++ name 
      putStrLn  msg
      return 

-}
{-
:t putStrLn
putStrLn :: String -> IO ()
:t getLine
getLine :: IO String
:t (++)
(++) :: [a] -> [a] -> [a]
type String = [Char]
-}

