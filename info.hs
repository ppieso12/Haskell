map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

map f xs is the list obtained by applying f to each element of xs, i.e.,

map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
map f [x1, x2, ...] == [f x1, f x2, ...]


elem_nr (h:_) 0 = h
elem_nr (_:t) i = elem_nr t (i-1)

dlugosc [] = 0
dlugosc (_:t) = 1 + dlugosc t

konkatenacja [] l2 = l2
konkatenacja (h:t) l2 = h : (konkatenacja t l2)

(+++) :: [a] -> [a] -> [a]
[] +++ lb = lb
(x:t) +++ lb = x:(+++) t lb
infixr 5 +++

KOMPOZYCJA map (not ... even) [1..5]
(...) :: (b->c) -> (a->b) -> a -> c
(...) f y = \x -> f $y x
infixr 9 ...

MAP ZA POMOCĄ FOLDR
map' :: (a->b) -> [a] -> [b]
map' f []       = []
-LUB- map' f xs       = foldr (\x xs -> (f x):xs) [] xs
map' f xs = foldr ((:).f) [] xs

LAMBDA EXPRESSION - anonimowa klasa wew w javie \te pierwsze to parametry
map (\x -> x+1) [1..5]

Types that can act like a box can be functors. You can think of a list as a box that has an infinite amount of little compartments 
and they can all be empty, one can be full and the others empty or a number of them can be full. 
So, what else has the properties of being like a box? For one, the Maybe a type. In a way, it's like a box that can either 
hold nothing, in which case it has the value of Nothing, or it can hold one item, like "HAHA", in which case it has a value of Just "HAHA"

class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
    
instance Functor [] where  
    fmap = map  
