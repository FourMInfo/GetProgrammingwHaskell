
import Data.Char

myGCD x y = if remainder == 0
              then y 
              else myGCD y remainder
            where remainder = x `mod` y

myHead (x:xs) = x
myHead [] = error "No head for empty list"

myTail (_:xs) = xs
myTail [] = error "No tail for empty list"

myGCDR x 0 = x
myGCDR x y = myGCDR y (x `mod` y)

myLength [] = 0
--myLength x = 1 + myLength (tail x)
myLength (x:xs) = 1 + myLength xs

--myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
--myTake n xs = if n >= length xs
--                   then xs
--                   else myTake n xy
--                     where xy = reverse (drop 1 (reverse xs))
myTake n (x:xs) = x:rest
                  where rest = myTake (n-1) xs

finiteCycle (first:rest) = first:rest ++ [first]                
--myCycle (x:xs) = new ++ myCycle (xs ++ new)
--                  where new = (x:xs) ++ [x]
myCycle (first:rest) = first:myCycle (rest ++ [first])


ackerman 0 n = n + 1
ackerman m 0 = ackerman (m-1) 1
ackerman m n = ackerman (m-1) (ackerman m (n-1))

collatz 1 = 1
collatz n = if even n 
             then 1 + collatz (n `div` 2)
             else 1 + collatz (n*3 + 1)

myReverse [] = []
myReverse [x] =[x]
myReverse xs = xs!!last : myReverse (take last xs)
                where last = length xs-1

myReverse2 [] = []
myReverse2 [x] = [x]
myReverse2 (x:xs) = myReverse xs ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibFast _ _ 0 = 0
fibFast _ _ 1 = 1
fibFast _ _ 2 = 1
fibFast n1 n2 c = n1 + fibFast n2 (n1 + n2) (c-1)

-- author's solution
fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y c = fastFib (x + y) x (c - 1)

fibFast2 _ _ 0 = 0
fibFast2 _ _ 1 = 1
fibFast2 _ _ 2 = 1
fibFast2 x y 3 = x + y
fibFast2 n1 n2 c = fibFast2 n2 (n1 + n2) (c-1)

fibFast3 _ _ 0 = 0
fibFast3 _ _ 1 = 1
fibFast3 x y 2 = x + y
fibFast3 n1 n2 c = fibFast3 n2 (n1 + n2) (c-1)

fastFibi = fibFast2 1 1

fastestFibi = fibFast3 0 1

addNA [] = []
addNA (x:xs) = (x ++ "a"):addNA xs

squareAll [] = []
squareAll (x:xs) = (x^2):squareAll xs

myMap f [] = []
myMap f (x:xs) = f x:myMap f xs

myFilter f [] = []
myFilter f (x:xs) = if f x 
                    then x:myFilter f xs
                    else myFilter f xs

myRemove f [] = []
myRemove f (x:xs) = if f x 
                    then myRemove f xs
                    else x:myRemove f xs
                    
myProduct [] = 1
myProduct (x:xs) = myProduct xs * x

myProductF xs = foldl (*) 1 xs

myConcatAll xs = foldl (++) "" xs

mySumOfSquares xs = foldl (+) 0 (map (^2) xs)

myRCons [] = []
myRCons (x:xs) = myRCons xs ++ [x]

rcons x y = y:x
myReverse3 xs = foldl rcons [] xs

myFoldL f init [] = init
myFoldL f init (x:xs) = myFoldL f (f init x) xs 

myFoldR f init [] = init
myFoldR f init (x:xs) = myFoldR f x (f init xs) 

myElem _ [] = False
myElem e (x:xs) = (e == x) || myElem e xs

myElem2 e (x:xs)  = length (filter (\x -> e == x) (x:xs)) >=1

myElem3 e xs  = length (filter (== e) xs) >=1

-- book solution
-- myElem val myList = (length filteredList) /= 0
--  where filteredList = filter (== val) myList

isPalindrome sentance = fSentance == reverse fSentance
              where fSentance = map toLower (filter (/= ' ') sentance)

harmonic n = foldr (+) 0 (map (1/) [1..n])

harmonic3 n = foldr ((+) . (1/)) 0 [1..n]

-- book version
harmonic2 n = sum (take n seriesValues)
          where seriesPairs = zip (cycle [1.0])  [1.0,2.0 .. ]
                seriesValues = map
                       (\pair -> (fst pair)/(snd pair))
                       seriesPairs