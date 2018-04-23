data Field = Empty
              | O
              | X
              deriving (Read, Eq, Show)

initBoard :: [[Field]]
initBoard = replicate 19 emptyline
         where 
            emptyline = replicate 19 Empty

changeField :: Int -> Int -> Field -> [[Field]] -> [[Field]]
changeField _ _ _ [] = []
changeField 0 y move (h:t) = ((exploreLine y h move)) : (changeField 20 20 move t)
changeField x y move (h:t) = h : (changeField (x - 1) y move t)

exploreLine :: Int -> [Field] -> Field -> [Field]
exploreLine _ [] _ = []
exploreLine 0 (h:t) field = field : (exploreLine (-1) t field)
exploreLine y (h:t) field = h : (exploreLine (y - 1) t field)
