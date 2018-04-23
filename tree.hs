import qualified Data.List
--https://github.com/ppieso12/Haskell/blob/master/info.hs
addval :: Num a => a -> a
addval aa = aa + 4

mymap :: (Num a, Ord a) => (a -> b) -> [a] -> [b]
mymap fun [] = []
mymap fun list = fun headd : map fun taill
     where 
     	headd = if head list < 5 then head list else 0
     	taill = tail list
--mymap fun (x:list) = fun x : map fun list
map' :: (a->b) -> [a] -> [b]
map' f []       = []
map' f xs       = foldr (\x newlis -> (f x):newlis) [] xs
--map' f xs = foldr ((:).f) [] xs

maximum' :: (Ord a) => [a] -> a  
maximum' (l:lll) = foldr (\x acc -> if x > acc then x else acc) l lll

reverse' :: [a] -> [a]  
reverse' l = foldl (\acc x -> x : acc) [] l

sum' :: Num a => [a] -> a
sum' list = foldl (\a b -> a+b) 0 list

myhead :: [a] -> a
myhead (h:_) = h

telem :: (Num a, Eq a) => [a] -> a -> a
telem (x:_) 0 = x
telem (_:t) n = telem t $n - 1

(+++) :: [a] -> [a] -> [a]
(+++) [] lb = lb
(+++) (x:t) lb = x:(+++) t lb
infixr 5 +++

($$) :: (a->b) -> a -> b
f $$ x = f x
infixr 0 $$
{- ALBO
(+++) :: [a] -> [a] -> [a]
[] +++ lb = lb
(x:t) +++ lb = x:(+++) t lb
infixr 5 +++
-}
(...) :: (b->c) -> (a->b) -> a -> c
(...) f y val = f (y val)
infixr 9 ...
--(...) f y = \x -> f $y x

wielomian :: Num a => [a] -> a -> a
wielomian coeff x = sum $ zipWith (*) coeff $ map (x^) [0..]

rep :: (Num a, Ord a) => a -> t -> [t]
rep a b
    | a == 0 = []
    | a > 0 = b:rep (a - 1) b


data Tree a = Nil
              | Node (Tree a) a (Tree a)
              deriving (Eq, Read, Show)

-- zrobic funkcje reduce funkcje map
{-
instance (Show a) => Show (Tree a) where
   --show (Node Nil) = "Nil"
   show (Node Nil x Nil) = show x -- ++ "\n" ++ "nil " ++ "nil "
   show (Node l x Nil) = " " ++ show x ++ "\n" ++ show l ++ "nil  "
   show (Node Nil x r) = " " ++ show x ++ "\n" ++ "nil " ++ show r	
   show (Node l x r) = " " ++ show x ++ "\n" ++ show l ++ "  " ++ show r
-}


data STree a = SEmpty | SLeaf a | SBranch a (STree a) (STree a)
		deriving (Eq, Read, Show)

convTree :: Tree a -> STree a
convTree (Nil) = SEmpty
convTree (Node Nil x Nil) = SLeaf x
convTree (Node l x r) = SBranch x (convTree l) (convTree r)
 


height :: Tree a -> Integer
height Nil = 0
height (Node left _ right) = 1 + max (height left) (height right)

istree :: Ord a => Tree a -> Bool
istree Nil = True
istree (Node t_left x t_right) = istree t_left &&
								 istree t_right &&
					(t_left == Nil || maxt t_left < x) &&
					(t_right == Nil || x < mint t_right)

maxt :: Ord a => Tree a -> a
maxt (Node t1 x t2) = max x (max y z)
	where
		y = if (t1 == Nil) then x
			else maxt t1
		z = if(t2 == Nil) then x
			else maxt t2

mint :: Ord a => Tree a -> a
mint (Node t1 x t2) = min x (min y z)
	where
		y = if (t1 == Nil) then x
			else mint t1
		z = if(t2 == Nil) then x
			else mint t2


search :: Ord a => Tree a -> a -> Bool
search Nil v = False
search (Node tl x tr) v
	| x == v	= True
	| v < x		= search tl v
	| otherwise = search tr v


insert :: Ord a => Tree a -> a -> Tree a 
insert Nil v = Node Nil v Nil
insert (Node tl x tr) v
    | x == v	= Node tl x tr
    | v < x		= Node (insert tl v) x tr
    | otherwise	= Node tl x (insert tr v)


makeTree :: Ord a => [a] -> Tree a 
makeTree [] = Nil
makeTree (x:xs) = insert (makeTree xs) x 
--LUB
--makeTree = foldl insert Nil  

{- makeTree [] = Nil
makeTree (x:xs) = insert (makeTree xs) x 
-}
--

inorder :: Tree a -> [a]
inorder Nil = []
inorder (Node tl x tr) = inorder tl
				++ [x] ++ inorder tr





data List' a = Empty
				| Elem a (List' a)
				deriving (Show, Eq)

take_elem :: List' a -> Int -> a 
take_elem (Elem x tr) 0 = x
take_elem (Elem x tr) num = take_elem tr (num - 1) 

last' :: List' a -> a
last' (Elem x Empty) = x
last' (Elem x tr) = last' tr 

init' :: List' a -> List' a
init' (Elem x Empty) = Empty
init' (Elem x tr) = Elem x (init' tr)

insert_list :: Ord a => List' a -> a -> List' a
insert_list Empty val = Elem val Empty
insert_list (Elem x tr) val = Elem x (insert_list tr val)

makeList ::Ord a => [a] -> List' a
makeList = foldl insert_list Empty

--makeList [] = Empty
--makeList (x:xs) = insert_list (makeList xs) x

--makeList = foldl insert_list Empty

map_list :: (a->b) -> List' a -> List' b
map_list f (Empty) = Empty
map_list f (Elem x tr) = Elem (f x) (map_list f tr)

foldl' :: (b -> a -> b) -> b -> List' a -> b     --Zaczyna od lewej w f bierze b na początku
foldl' f start (Elem x Empty) = f start x
foldl' f start (Elem x tr) = foldl' f (f start x) tr

foldr' :: (a -> b -> b) -> b -> List' a -> b     
foldr' f start (Elem x Empty) = f x start 
foldr' f start list = foldr' f (f (last' list) start) (init' list)

zipWith' :: (a -> b -> c) -> List' a -> List' b -> List' c
zipWith' _ Empty _ = Empty  
zipWith' _ _ Empty = Empty  
zipWith' f (Elem x tx) (Elem y ty) = Elem (f x y) (zipWith' f tx ty) 

(++++) :: List' a -> List' a -> List' a
Empty ++++ lb = lb
(Elem x tx) ++++ lb = Elem x ((++++) tx lb) 
infixr 5 ++++

fromNormalList :: [a] -> List' a
fromNormalList [] = Empty
fromNormalList (x:xs) = Elem x (fromNormalList xs)

toNormalList :: List' a -> [a]
toNormalList Empty = []
toNormalList (Elem x tr) = x : (toNormalList tr)

instance Functor List' where
    fmap f Empty = Empty
    fmap f (Elem x tr) = Elem (f x) (fmap f tr)

instance Applicative List' where
    pure x = Elem x Empty
    Empty <*> _ = Empty
    (Elem f tr) <*> sth = fmap f sth




data MyTree a = Null
               | Leaf a (MyTree a)  --- jeden poziom liści
               | Branch a (MyTree a) (MyTree a) -- gałąż ktora ma liscie i dostep do nastepnego poziomu Branch x liście Branch+1
               deriving (Show)

gen :: Int -> Int -> a -> MyTree a
gen 0 _ _ = Null
gen deep branches x = Branch x (makebranches branches x) (gen (deep-1) branches x) 


makebranches :: Int -> a -> MyTree a
makebranches 0 x = Leaf x Null
makebranches branches x = Leaf x (makebranches (branches-1) x)


--class Functor' f where
 -- fmap' :: (a -> b) -> f a -> f b


instance Functor MyTree where
    fmap f Null = Null
    fmap f (Leaf lf lfother) = Leaf (f lf) (fmap f lfother)
    fmap f (Branch x (Leaf lf lfother) nextLvl) = Branch (f x) (Leaf (f lf) (fmap f lfother)) (fmap f nextLvl) 



ff :: Num a => [a] -> [a]
ff [] = []
ff (x:xs) = ff xs ++ [x]



wczytaj :: IO Int
wczytaj = do
        liczba <- getLine
        let wynik = sumuj liczba
        return wynik


sumuj :: String -> Int
sumuj [] = 0
sumuj (x:xs) = ((fromEnum x) -48) + sumuj xs
