module Main where

eitherHead :: [a] -> Either String a
eitherHead [] = Left "There is no head because the list is empty"
eitherHead (x:xs) = Right x

intExample :: [Int]
intExample = [1,2,3]

intExampleEmpty :: [Int]
intExampleEmpty = []

charExample :: [Char]
charExample = "cat" 

charExampleEmpty :: [Char]
charExampleEmpty = "" 

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake n xs = (head xs) : myTake (n-1) (tail xs)


myTakePM :: Int -> [a] -> [a]
myTakePM 0 _ = []
myTakePM n (x:xs) = x : (myTakePM (n-1) xs) 
myTakePM _ [] = []

data PrimeError = TooLarge | InvalidValue

instance Show PrimeError where
   show TooLarge = "Value exceed max bound"
   show InvalidValue = "Value is not a valid candidate for prime checking"

isPrime :: Int -> Either PrimeError Bool
isPrime n
   | n < 2 = Left InvalidValue
   | n > maxN = Left TooLarge
   | otherwise = Right (n `elem` primes)


primes :: [Int]
primes = [2,3,5,7]

maxN :: Int
maxN = 10

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError


myTakeSafer :: Int -> Maybe [a] -> Maybe [a]
myTakeSafer 0 _ = Just []
myTakeSafer n (Just xs) = (:) <$> (maybeHead xs)
                              <*> myTakeSafer (n-1) (Just (tail xs))


main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- (read <$> getLine)
  let result = isPrime n
  print (displayResult result)
  
