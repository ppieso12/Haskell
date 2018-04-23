import qualified Data.List
import Data.List
import Data.Ord
import System.Random (randomRIO)
import System.IO
--https://github.com/micwypych/java-vs-haskell
--http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html

main :: IO ()
main = do
    putStrLn "Inicjalizacja"
    let board = initBoard
    let board_tmp = initBoard
    game <- gamevv "Witaj! "      --Rodzaj Gry - OPISY FUNKCJI ZNAJDUJA SIE PRZY ICH DEFINICJACH            

    first_stone <- stoneFirst ""  --Wczytanie Kamienia dla 1 gracza

    let second_stone = if first_stone == X then O else X

    if game == 0 then putStrLn "Gra rozpoczeta z człowiekiem!" else putStrLn "Gra rozpoczeta z AI!"
    showBoard board
    if game == 0 then  mainloopHuman 1 board board_tmp first_stone second_stone else mainloopAI board board_tmp first_stone second_stone
   
mainloopAI :: [[Field]] -> [[Field]] -> Field -> Field -> IO ()
mainloopAI board board_tmp first_stone second_stone = do

                        putStrLn "Ruch gracza: " 
                        putStrLn $ show first_stone 
                       
                        move_tuple <- move board
      
                        let good_row = fst move_tuple
                        let good_col = snd move_tuple
 
                        let board_tmp = changeField good_row good_col first_stone board 
                        let board = board_tmp

                        if (check_winner good_row good_col first_stone board) then end_game board "Wygrałeś! Gratulacje dla " True first_stone else end_game board "" False first_stone
                                     
                        --putStrLn "Ruch AI" 
                        --putStrLn "Wzgledem X"
                        let forX = maximportant ( important ( fun board (getAvailableCells board) first_stone)) --SZUKANIE NAJBARDZIEJ OPTYMALNYCH RUCHOW W DANEJ CHWILI DLA SIEBIE LUB 
                        --putStrLn $ show forX                                                                  --BLKOWANIE PRZECIWNIKA GDY GROZI TO PRZEGRANA
                        --putStrLn "Wzgledem O" 
                        let forO = maximportant ( important ( fun board (getAvailableCells board) second_stone))
                        --putStrLn $ show forO
                        --putStrLn "Lepsze" 
                        let bettermoves = whichbetter forX forO --Atakować przeciwnika czy dostawiać sobie i gonić do wygranej
                        --putStrLn $ show bettermoves

                        let indexmax = (length bettermoves) - 1
                        let rangee = randomRIO (0,indexmax)  --Losowe wybranie pól o takim samym priorytecie (jeśli są) - najbardziej optymalne do wyboru
                        range <- rangee
                        let bettermove = bettermoves !! range
                        --putStrLn $ show bettermove

                        let board_tmp = changeField (first bettermove) (second bettermove) second_stone board
                        let board = board_tmp

                        showBoard board

                        if (check_winner (first bettermove)  (second bettermove) second_stone board) then end_game board "Przykro mi! Klęska dla " True first_stone else end_game board "" False first_stone

                        if (check_winner (first bettermove)  (second bettermove) second_stone board)||(check_winner good_row good_col first_stone board) then return () 
                            else  mainloopAI board board_tmp first_stone second_stone 

mainloopHuman :: Int -> [[Field]] -> [[Field]] -> Field -> Field ->  IO ()
mainloopHuman player board board_tmp first_stone second_stone = do

                        putStrLn "Ruch gracza: " 
                        if (player `mod` 2) == 1 then putStrLn $ show first_stone 
                            else putStrLn $ show second_stone 
                        
                        move_tuple <- move board
                        let good_row = fst move_tuple
                        let good_col = snd move_tuple
                        
                        let board_tmp = if (player `mod` 2) == 1 then changeField good_row good_col first_stone board 
                            else changeField good_row good_col second_stone board

                        let board = board_tmp

                        let current_player = if (player `mod` 2) == 1 then first_stone else second_stone
                        showBoard board
                        if (check_winner good_row good_col current_player board) then end_game board "Wygrałeś! Gratulacje dla " True current_player else end_game board "" False first_stone

                        if (check_winner good_row good_col current_player board) then return () else  mainloopHuman (player + 1) board board_tmp first_stone second_stone 
    
--NOWE TYPY

data Field = Empty
              | O
              | X
              deriving (Read, Eq)

data Line = Line [Field]

data Board = Board [[Field]]

--ODPOWIEDNIE INSTANCJE SHOW

instance Show Field where
   show X = "X"
   show O = "O"
   show Empty = "_"

instance Show Line  where
   show (Line []) = "\n"
   show (Line (x:xs)) = show x ++ "   " ++ show (Line xs) 

instance Show Board where
   show (Board []) = "   1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19\n"
   show (Board (x:xs)) 
           | length xs < 10 = show (20 - ((+1) $ length xs)) ++ " " ++ show (Line x) ++ show (Board xs)
           | length xs >= 10 = show (20 - ((+1) $ length xs)) ++ "  " ++ show (Line x) ++ show (Board xs)

-- WCZYTYWANIE RUCHÓW, ZNAKÓW, PÓL, OBSŁUGA BŁĘDÓW W TYM KONIEC GRY

move ::[[Field]] -> IO (Int,Int)  -- Wczytywanie odpowiedniego ruchu + całościowa obsługa błędów
move board = do
    row <- move_prepare "Nr Wiersza: "
    col <- move_prepare "Nr Kolumny: "
    let good_row = dec row
    let good_col = dec col
    if isEmpty board good_row good_col then putStrLn "Poprawne dane, dzięki!" else putStrLn "Zajęte Miejsce!"
    if isEmpty board good_row good_col then return (good_row, good_col) else move board

move_prepare :: String -> IO Int --Czy zadano odp znaki i max 2 cyfrową liczbę
move_prepare str = do
    putStrLn "Podaj koordynaty ruchu: " 
    putStr str
    line <- getLine 
    let check_ascii = (((map (<58) (map fromEnum line)) == (map (>47) (map fromEnum line))) && (((length line) < 3) && ((length line) > 0)))

    range str check_ascii (read line :: Int)

range :: String -> Bool -> Int -> IO Int  -- czy zakres tej liczby sie zgadza
range str False _ = do
                    putStr "Błąd! "
                    move_prepare str
range str True number 
                    | (number >= 1) && (number < 20) = do
                        return number 

                    | otherwise = do
                        putStr "Błąd! "
                        move_prepare str

stoneFirst :: String -> IO Field  -- obsługa błędu przy wyborze znaku kamienia dla gracza
stoneFirst str = do
    putStr str
    putStr "Wybor kamienia dla pierwszego gracza: [X lub O] "
    line <- getLine 
    let check_ascii = (((fromEnum (line !! 0)) == 88)||((fromEnum (line !! 0)) == 79)) && ((length line) == 1)
    if check_ascii then return (read line :: Field) else stoneFirst "Błąd! "

gamevv :: String -> IO Int  -- obsługa błędu przy wyborze gry
gamevv str = do
    putStr str
    putStrLn "Rodzaj gry - 0 człowiek - 1 komputer"
    line <- getLine 
    let check_ascii = (((map (<=49) (map fromEnum line)) == (map (>=48) (map fromEnum line))) && ((length line) == 1))
    if check_ascii then return (read line :: Int) else gamevv "Błąd! "

check_winner :: Int -> Int -> Field -> [[Field]] -> Bool --Sprawdzenie zwyciestwa, >=5 takich samych znakow w lini poziomej, pionowej lub na ukos
check_winner x y field m = ((go_horizontal x y field m) 
                        || (go_vertical x y field m)
                        || (go_upleft_downright x y field m) 
                        || (go_downleft_upright x y field m))

end_game :: [[Field]] -> String -> Bool -> Field -> IO ()  --funkcja przetwarzająca zdarzenie końca gry 
end_game _ _ False _ = return () --brak końca gry, gramy dalej 
end_game b str True field = do  --możliwość kontynuacji od początku lub wyjście z wszystkich funkcji - zakończenie programu
                    putStr str
                    putStr (show field)
                    decision <- ifagain ""
                    if decision == 121 then main else return ()

ifagain :: String -> IO Int --obłsuga błędów decyzji nowej gry 
ifagain str = do
            putStr str
            putStrLn " - Czy chcesz zagrać od nowa? - y -> tak - n -> nie"
            line <- getLine 
            let check_ascii = (((fromEnum (line !! 0)) == 121)||((fromEnum (line !! 0)) == 110)) && ((length line) == 1)
            if check_ascii then return (fromEnum (line !! 0)) else ifagain "Błąd! "

--GŁÓWNE FUNCKJE

initBoard :: [[Field]] 
initBoard = replicate 19 emptyline
         where 
            emptyline = replicate 19 Empty

dec :: (Num a) => a -> a
dec x = x - 1 

isEmpty :: [[Field]] -> Int -> Int -> Bool  -- czy wolne pole w boardzie
isEmpty m x y = if (m !! x !! y) == Empty then True else False

showBoard :: [[Field]] -> IO () 
showBoard [] = putStrLn ""
showBoard x = putStrLn $ show (Board x) 

changeField :: Int -> Int -> Field -> [[Field]] -> [[Field]]   -- Kolejno 2 funckje odpowiadające za modyfikację planszy i tworzenie nowej
changeField _ _ _ [] = []
changeField 0 y move (h:t) = ((exploreLine y h move)) : (changeField 20 20 move t)
changeField x y move (h:t) = h : (changeField (x - 1) y move t)

exploreLine :: Int -> [Field] -> Field -> [Field]
exploreLine _ [] _ = []
exploreLine 0 (h:t) field = field : (exploreLine (-1) t field)
exploreLine y (h:t) field = h : (exploreLine (y - 1) t field)

--SPRAWDZANIE ZWYCIESTWA

go_vertical :: Int -> Int -> Field -> [[Field]] -> Bool
go_vertical x y field m = if ((go_up x y field m True) + (go_down x y field m True) - 1 ) >= 5 then True else False  -- -1 bo dwie z tego samego miejsca

go_horizontal :: Int -> Int -> Field -> [[Field]] -> Bool
go_horizontal x y field m = if ((go_left x y field m True) + (go_right x y field m True) - 1 ) >= 5 then True else False

go_upleft_downright :: Int -> Int -> Field -> [[Field]] -> Bool
go_upleft_downright x y field m = if ((go_left_up x y field m True) + (go_right_down x y field m True) - 1 ) >= 5 then True else False

go_downleft_upright :: Int -> Int -> Field -> [[Field]] -> Bool
go_downleft_upright x y field m = if ((go_left_down x y field m True) + (go_right_up x y field m True) - 1 ) >= 5 then True else False

go_up :: Int -> Int -> Field -> [[Field]] -> Bool -> Int 
go_up _ _ _ _ False = 0                                     --falsz porownania, koniec
go_up 0 y field m True = 1  -- ostatnie porownanie, dla razem 4 porownania bo poczatkowe ma domyslnie true
go_up x y field m True = 1 + (go_up (x-1) y field m (field == m !! (x-1) !! y))

go_down :: Int -> Int -> Field -> [[Field]] -> Bool -> Int 
go_down _ _ _ _ False = 0                                        --wyjscie poza zakres, mniej niz 5 wystapien kamienia
go_down 18 y field m True = 1 
go_down x y field m True = 1 + (go_down (x+1) y field m (field == m !! (x+1) !! y))

go_left :: Int -> Int -> Field -> [[Field]] -> Bool -> Int 
go_left _ _ _ _ False = 0                                       
go_left x 0 field m True = 1  
go_left x y field m True = 1 + (go_left x (y-1) field m (field == m !! x !! (y-1)))

go_right :: Int -> Int -> Field -> [[Field]] -> Bool -> Int 
go_right _ _ _ _ False = 0                                      
go_right x 18 field m True = 1 
go_right x y field m True = 1 + (go_right x (y+1) field m (field == m !! x !! (y+1)))

go_left_up :: Int -> Int -> Field -> [[Field]] -> Bool -> Int 
go_left_up _ _ _ _ False = 0        
go_left_up 0 y field m True = 1                                     
go_left_up x 0 field m True = 1  
go_left_up x y field m True = 1 + (go_left_up (x-1) (y-1) field m (field == m !! (x-1) !! (y-1)))

go_left_down :: Int -> Int -> Field -> [[Field]] -> Bool -> Int 
go_left_down _ _ _ _ False = 0    
go_left_down 18 y field m True = 1                                     
go_left_down x 0 field m True = 1  
go_left_down x y field m True = 1 + (go_left_down (x+1) (y-1) field m (field == m !! (x+1) !! (y-1)))

go_right_up :: Int -> Int -> Field -> [[Field]] -> Bool -> Int 
go_right_up _ _ _ _ False = 0    
go_right_up 0 y field m True = 1                                     
go_right_up x 18 field m True = 1  
go_right_up x y field m True = 1 + (go_right_up (x-1) (y+1) field m (field == m !! (x-1) !! (y+1)))

go_right_down :: Int -> Int -> Field -> [[Field]] -> Bool -> Int 
go_right_down _ _ _ _ False = 0    
go_right_down 18 y field m True = 1                                     
go_right_down x 18 field m True = 1  
go_right_down x y field m True = 1 + (go_right_down (x+1) (y+1) field m (field == m !! (x+1) !! (y+1)))

--IMPLEMENTACJA AI--

getAvailableCells :: [[Field]] -> [(Int, Int)] --WSZYSTKIE DOSTEPNE WOLNE MIEJSCA NA PLANSZY
getAvailableCells m = getRowCells m 0

getRowCells :: [[Field]] -> Int -> [(Int, Int)]
--getRowCells m x False = [] ++ (getRowCells m (x+1) (isEmpty m x 0)
getRowCells m 18 = getCells m 18 0 (isEmpty m 18 0)
getRowCells m x = (getCells m x 0 (isEmpty m x 0)) ++ (getRowCells m (x+1))

getCells :: [[Field]] -> Int -> Int -> Bool -> [(Int, Int)]
getCells m x 18 False = []
getCells m x y False = [] ++ (getCells m x (y+1) (isEmpty m x (y+1)) )
getCells m x 18 True = [(x,18)]
getCells m x y True = (x,y) : (getCells m x (y+1) (isEmpty m x (y+1)) )

three :: (Int, Int, Int) -> Int
three (_,_,a) = a
first :: (Int, Int, Int) -> Int
first (a,_,_) = a
second :: (Int, Int, Int) -> Int
second (_,a,_) = a

fun :: [[Field]] -> [(Int, Int)] -> Field -> [(Int, Int, Int)] -- TWORZY KROTKE Z 3 MIEJSCAMI ODPOWIEDNIO (POZ X, POZ Y, KOSZT/PRIORYTET)
fun _ [] _= []
fun m (x:xs) field = (fst x, snd x, (value (fst x) (snd x) field m)) : (fun m xs field)

important :: [(Int, Int, Int)]-> [(Int, Int, Int)]   --Zwraca liste wolnych miejsc bez kosztu 0 
important [] = []
important (x:xs) = if (three x) == 0 then important xs else x : (important xs)

maximportant :: [(Int, Int, Int)] -> [(Int, Int, Int)] -- RUCHY NAJBARDZIEJ OPŁACALNE I NAJWYZEJ PUNKTOWANE
maximportant m = maximportant_list m (maxval m 0)

maximportant_list :: [(Int, Int, Int)] -> Int -> [(Int, Int, Int)]
maximportant_list [] _ = []
maximportant_list (x:xs) maxvv = if (three x) == maxvv then x : (maximportant_list xs maxvv) else maximportant_list xs maxvv

maxval :: [(Int, Int, Int)] -> Int -> Int   --Zwraca max wartosc kosztu ze wszystkich mozliwych ruchow
maxval [] highest = highest
maxval (x:xs) highest = if highest < (three x) then maxval xs (three x) else maxval xs highest

value :: Int -> Int -> Field -> [[Field]] -> Int
value x y field m = maximum [((go_up x y field m True) + (go_down x y field m True) - 2),
                    ((go_left x y field m True) + (go_right x y field m True) - 2),      
                    ((go_left_up x y field m True) + (go_right_down x y field m True) - 2),
                    ((go_left_down x y field m True) + (go_right_up x y field m True) - 2)]
                    
whichbetter :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)] -- czy dostawic sobie czy atakowac przeciwnika?
whichbetter a b = if (maxval a 0 ) > (maxval b 0) then a else b