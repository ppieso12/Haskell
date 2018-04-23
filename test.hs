import Control.Monad -- mapM
import System.Environment -- getArgs




main :: IO ()
main = do
    putStrLn "Podaj nazwę pliku: "
    name <- getLine
    let filename = name :: String

    --putStrLn "Podaj ilo: "
   -- args <- getArgs
    putStrLn "Podaj argumenty do wyszukania w pliku rozdzielone spacja np: aa bb cc : "
    line <- getLine

    let args = words line

    print args
    content <- readFile filename--"foo.txt"

    let allwords_list = words content

    let tuples = czeck_for_every_arg args allwords_list 

    print tuples

    
czeck_for_every_arg :: [String] -> [String] -> [(String, Int)] -- 
czeck_for_every_arg [] _ = []
czeck_for_every_arg (arg:others) document = (check_words_amount document arg 0):(czeck_for_every_arg others document)

check_words_amount :: [String] -> String -> Int ->  (String, Int)  ---dokument do analizy -> słowo szukane -> jego ilość -> zwracana var
check_words_amount [] phrase amount = (phrase,amount)
check_words_amount (filex:filexs) phrase amount = if filex == phrase then check_words_amount filexs phrase (amount+1) else check_words_amount filexs phrase amount

  






