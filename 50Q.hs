module Questoes where

main = undefined

myenumFromTo :: Int -> Int -> [Int]
myenumFromTo a b = if (a < b) then a : myenumFromTo (a+1) b else [b]

myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo a b c = if (a<c) then a : myenumFromThenTo (a+b) b c else [c]

plusplus :: [a] -> [a] -> [a]
plusplus [] b  = b			
plusplus a []  = a
plusplus (h:t) a = h : plusplus t a

posL :: [a] -> Int -> a
posL (h:t) n = if (n>0) then posL t (n-1) else h

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (h:t) = (myreverse t) ++ [h]

mytake :: Int -> [a] -> [a]
mytake n [] = []
mytake n (h:t) = if (n>0) then h : mytake (n-1) t else []

mydrop :: Int -> [a] -> [a]
mydrop n [] = []
mydrop n (h:t) = if (n>0) then mydrop (n-1) t else (h:t)

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = [(x,y)] ++ myzip xs ys

myelem :: Eq a => a -> [a] -> Bool
myelem n [] = False
myelem n (h:t) = if (n==h) then True else myelem n t 

myreplicate :: Int -> a -> [a]
myreplicate n a = if (n>0) then [a] ++ myreplicate (n-1) a else []

myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse a (h:t) = [h] ++ [a] ++ myintersperse a t

mygroup :: Eq a => [a] -> [[a]]
mygroup [] = []
mygroup l@(h:t) = mytake (mygroupaux l) l : mygroup (mydrop (mygroupaux l) l) 

mygroupaux :: Eq a => [a] -> Int
mygroupaux [] = 0
mygroupaux [a] = 1
mygroupaux (a:b:c) = if (a==b) then 1 + mygroupaux (b:c) else 1