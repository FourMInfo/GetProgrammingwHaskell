data FourLetterAlphabet = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)
data ThreeLetterAlphabet = Alpha | Beta | Kappa  deriving (Show,Enum,Bounded)


rotEncoder ::(Bounded a, Enum a) => Int -> a -> a
rotEncoder alphaSize letter =
     toEnum ((fromEnum letter + halfSize) `mod` alphaSize)
         where halfSize = alphaSize `div` 2

-- Book version:
-- rotN :: (Bounded a, Enum a) => Int -> a -> a
-- rotN alphabetSize c = toEnum rotation
--  where halfAlphabet = n `div` 2
--        offset = fromEnum c + halfAlphabet
--        rotation =  offset `mod` alphabetSize

largestChar = fromEnum (maxBound :: Char)
lowestChar = fromEnum (minBound :: Char)

charEncoder :: Char -> Char,j
charEncoder  = rotEncoder alphaSize  
                    where alphaSize = largestChar + 1

fourLetEncoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetEncoder = map (rotEncoder 4)

stringEncoder :: String -> String
stringEncoder = map (rotEncoder $ largestChar + 1)
--or 
-- stringEncoder = map charEncoder

--  rotN for odd
rotDecoder ::(Bounded a, Enum a) => Int -> a -> a
rotDecoder alphaSize letter =
    toEnum ((fromEnum letter + offset) `mod` alphaSize)
        where halfSize = alphaSize `div` 2
              offset
                | even alphaSize = halfSize
                | otherwise = halfSize + 1
            
threeLetEncoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetEncoder = map (rotEncoder 3)

threeLetDecoder :: [ThreeLetterAlphabet] -> [ThreeLetterAlphabet]
threeLetDecoder = map (rotDecoder 3)

fourLetDecoder :: [FourLetterAlphabet] -> [FourLetterAlphabet]
fourLetDecoder = map (rotDecoder 4)

stringDecoder :: String -> String
stringDecoder = map (rotDecoder $ largestChar + 1)

xor :: Bool -> Bool -> Bool
xor a b = a /= b
-- book version
-- xorBool :: Bool -> Bool -> Bool
-- xorBool value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool, Bool) -> Bool
xorPair (a,b) = xor a b

-- called xor in book
xorList :: [Bool] -> [Bool] -> [Bool]
xorList l1 l2 = map xorPair (zip l1 l2)

type Bits = [Bool]
intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n 
    | n `mod` 2 == 0  = False : intToBits' nextVal
    | otherwise = True : intToBits' nextVal
    where nextVal = n `div` 2

maxBits :: Int
maxBits = length (intToBits' maxBound)

intToBits :: Int -> Bits
intToBits n = leadingBits ++ reverseBits
                where reverseBits = reverse $ intToBits' n
                      leadingBits = replicate (maxBits - length reverseBits)  False

-- book version
-- intToBits :: Int -> Bits
-- intToBits n = leadingFalses ++ reversedBits
--    where reversedBits = reverse  (intToBits' n)
--          missingBits = maxBits - (length reversedBits)
--          leadingFalses = take missingBits (cycle [False])

charToBits :: Char -> Bits
charToBits letter = intToBits (fromEnum letter)

power2 :: (Bool,Int) -> Int
power2 (b,n)
    | b = 2^n
    | otherwise = 0

bitsToChar :: Bits -> Char
--bitsToChar n = toEnum $ foldl (+) 0 (map power2 (zip (reverse n) [0..]))
-- hlint suggestion to use sum
bitsToChar n = toEnum $ sum $ map power2 $ zip (reverse n) [0..]

-- book solution
-- bitsToInt :: Bits -> Int
-- bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
--  where size = length bits
--        indices = [size-1,size-2 .. 0]
--        trueLocations = filter (\x -> fst x == True)
--                        (zip bits indices)
-- bitsToChar :: Bits -> Char
-- bitsToChar bits = toEnum (bitsToInt bits)

stringToBits :: String -> [Bits]
stringToBits = map charToBits

bitsToString :: [Bits] -> String
bitsToString = map bitsToChar

-- xorBits :: [Bits] -> [Bits] -> [Bits]
-- xorBits [] [] = []
-- xorBits (s1:xs1) (s2:xs2) = xorList s1 s2 : xorBits xs1 xs2

xorStrings :: String -> String -> String
xorStrings s1 s2 = bitsToString $ xorBits (stringToBits s1) (stringToBits s2)
                    where
                        xorBits [] [] = []
                        xorBits _  [] = []
                        xorBits (s1:xs1) (s2:xs2) = xorList s1 s2 : xorBits xs1 xs2

-- book version
-- applyOTP' :: String -> String -> [Bits]
-- applyOTP' pad plaintext =  map (\pair ->
--                                  (fst pair) `xor` (snd pair))
--                            (zip padBits plaintextBits)
--     where padBits =  map charToBits pad
--           plaintextBits =  map charToBits plaintext
-- applyOTP :: String -> String -> String
-- applyOTP pad plaintext = map bitsToChar bitList
--     where bitList = applyOTP' pad plaintext

myPad = "shhhhhh"

xorEncDec :: String -> String
xorEncDec = xorStrings myPad
