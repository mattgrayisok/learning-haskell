doubleMe :: (Num a) => a -> a
doubleMe x = x * 2
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
			then x
			else doubleMe x 
lucky :: (Integral x) => x -> String
lucky 7 = "You gave me a seven"
lucky y = "Nope"

describe :: (Show a) => [a] -> String
describe [] = "That's empty mate"
describe [x] = "One value is " ++ show x
describe [x,y] = "Two values " ++ show x ++ " and " ++ show y
describe (x:y:z:a:_) = "Many values " ++ show z

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:y) = x + sum y

max' :: (Ord a) => [a] -> a
max' [] = error "Empty list"
max' [a] = a
max' (a:xb) 
    | a < (max' xb) = max' xb
    | otherwise = a

initials :: String -> String -> String
initials (f:_) (l:_) = "You are " ++ [f] ++ " " ++ [l]

bmi :: (RealFloat a) => [(a,a)] -> [a]
bmi xs = [bmi | (h, w) <- xs, let bmi = w / h ^ 2, bmi >= 0.5]

contrived :: (Ord a,Num a) => a -> a -> a
contrived a b 
              | product < 5 = a
              | product >= 5 = b
              where product = a * b 

contrived' :: (Ord a,Num a) => a -> a -> a
contrived' a b = 
                 let product = a * b 
                 in product

fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

biggest :: (Ord a) => [a] -> a
biggest [] = error "EMPTY = NO BIG"
biggest [a] = a
biggest (a:xb) | a > nextbig = a
               | otherwise = nextbig
               where nextbig = biggest xb

biggest' :: (Ord a) => [a] -> a
biggest' [a] = a
biggest' (x:xs) = max x (biggest' xs)

replicate' :: (Integral a) => a -> a -> [a]
replicate' 0 a = []
replicate' b a = replicate' (b-1) a ++ [a]

take' :: (Integral a) => a -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

rev' :: (Num a) => [a] -> [a]
rev' [] = []
rev' (x:xs) = rev' xs ++ [x]

compareWith :: (Ord a) => a -> a -> Ordering
compareWith x y = compare x y

-- Determine if a list is sequential
issequential :: (Ord a) => [a] -> Bool
issequential [] = True
issequential [x,y] = x <= y
issequential (x:y:xs) | x <= y = issequential (y:xs)
                      | otherwise = False

-- A single bubble sort pass, could be defined in a where on the main bubble func
bubblePass :: (Ord a) => [a] -> [a]
bubblePass [] = []
bubblePass [x,y] | x > y = [y,x]
                 | otherwise = [x,y]
bubblePass (a:b:xs) | a > b = b : (bubble (a : xs)) 
                    | otherwise = a : (bubble (b : xs))

-- The main bubble function which runs passes until sequential
bubble :: (Ord a) => [a] -> [a]
bubble [] = []
bubble a | issequential a = a
         | otherwise = bubble (bubblePass a)

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain a | odd a = a : chain (a*3+1)
        | otherwise = a : chain (div a 2)
