map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

map f xs is the list obtained by applying f to each element of xs, i.e.,

map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
map f [x1, x2, ...] == [f x1, f x2, ...]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 
Input: zipWith (+) [1,2,3] [3,2,1]
Output: [4,4,4]

it evaluates the function flipping the order of arguments 
(a -> b -> c) -> b -> a -> c
Input: flip (>) 3 5

Output: True


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

data Maybe a = Nothing | Just a

class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
    
instance Functor Maybe where
    fmap func (Just val) = Just (func val)
    fmap func Nothing = Nothing
instance Functor Tree where  
        fmap f EmptyTree = EmptyTree  
        fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)      
    
    
> fmap (+3) (Just 2)
Just 5
  
fmap (getPostTitle) (findPost 1)
If findPost returns a post, we will get the title with getPostTitle. If it returns Nothing, we will return Nothing! Pretty neat, huh?
<$> is the infix version of fmap, so you will often see this instead:
getPostTitle <$> (findPost 1)

instance Functor ((->) r) where
    fmap f g = f . g

When you use fmap on a function, you're just doing function composition!

instance Functor [] where  
    fmap = map  
APPLICATIVE
<*> is left-associative
Control.Applicative defines <*>, which knows how to apply a function wrapped in a context to a value wrapped in a context:
Just (+3) <*> Just 2 == Just 5
Using <*> can lead to some interesting situations. For example:
> [(*2), (+3)] <*> [1, 2, 3]
[2, 4, 6, 4, 5, 6]

class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  

instance Applicative Maybe where  
    pure x = Just x 
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something

ghci> pure (+) <*> Just 3 <*> Just 5  
Just 8

(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x

By using <$>, the applicative style really shines, because now if we want to apply a function f between three applicative functors
, we can write f <$> x <*> y <*> z. If the parameters weren't applicative functors but normal values, we'd write f x y z.

ghci> (++) <$> Just "johntra" <*> Just "volta"  
Just "johntravolta" 

instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  

[ x*y | x <- [2,5,10], y <- [8,10,11]]  == (*) <$> [2,5,10] <*> [8,10,11] It's easy to see how pure f <*> xs equals fmap f xs with lists

class Monad m where  
    return :: a -> m a  
  
    (>>=) :: m a -> (a -> m b) -> m b  
  
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg 
    
instance Monad Maybe where  
return x = Just x  
Nothing >>= f = Nothing  
Just x >>= f  = f x  
fail _ = Nothing
    
    
instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []      
    
