import qualified Data.List

petla :: String -> [String] -> String -> IO ()
petla sep list = do
	line <- getLine 
	putStrLn (Show line)
	let new_str = sep ++ line
	let new_list = list ++ new_str
	putStrLn (Show new_list)
	petla sep new_list


main :: IO ()
main sep = do
    
    line <- getLine 
    let list = [line]
    petla sep list 


data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Eq)
data Ca = Ca String String Int
    deriving (Show)

instance Show Car where
    show (Car company model year)  = (company) ++" " ++ model++ " " ++ (show year) ++ "\n"