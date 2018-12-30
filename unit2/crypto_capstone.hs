data FourLetterAlpha = L1 | L2 | L3 | L4 deriving (Show,Enum,Bounded)

rotN :: (Bounded a, Enum a) => Int -> a -> a
rotN n c = toEnum rotation
 where halfN = n `div` 2
       offset = (fromEnum c) + halfN
       rotation =  offset `mod` n



largestCharNumber :: Int
largestCharNumber  = fromEnum (maxBound :: Char)  

rotChar :: Char -> Char
rotChar val = rotN nChar val
 where nChar = 1 + (fromEnum (maxBound :: Char))



fourLetterMessage :: [FourLetterAlpha]
fourLetterMessage = [L1,L3,L4,L1,L1,L2]


fourLetterEncoder :: [FourLetterAlpha] -> [FourLetterAlpha]
fourLetterEncoder vals = map rot4l vals
 where alphaSize = 1 + (fromEnum ( maxBound :: FourLetterAlpha))
       rot4l = rotN alphaSize

data ThreeLetterAlpha = Alpha | Beta | Kappa deriving (Show,Enum,Bounded)

threeLetterMessage :: [ThreeLetterAlpha]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]

threeLetterEncoder :: [ThreeLetterAlpha] -> [ThreeLetterAlpha]
threeLetterEncoder vals =  map rot3l vals
 where alphaSize = 1 + (fromEnum ( maxBound :: ThreeLetterAlpha))
       rot3l = rotN alphaSize

rotNdecoder :: (Bounded a, Enum a) => Int -> a -> a
rotNdecoder n c = toEnum rotation
 where halfN = n `div` 2
       offset = if even n
                then (fromEnum c) + halfN
                else 1 + (fromEnum c) + halfN
       rotation =  offset `mod` n

threeLetterDecoder :: [ThreeLetterAlpha] -> [ThreeLetterAlpha]
threeLetterDecoder vals =  map rot3ldecoder vals
 where alphaSize = 1 + (fromEnum ( maxBound :: ThreeLetterAlpha))
       rot3ldecoder = rotNdecoder alphaSize

rotEncoder :: String -> String
rotEncoder text = map rotChar text
 where alphaSize = 1 + (fromEnum (maxBound :: Char))
       rotChar = rotN alphaSize

rotDecoder :: String -> String
rotDecoder text =  map rotCharDecoder text
 where alphaSize = 1 + (fromEnum (maxBound :: Char))
       rotCharDecoder = rotNdecoder alphaSize

xor' :: Bool -> Bool -> Bool
xor' value1 value2 = (value1 || value2) && (not (value1 && value2))

xorPair :: (Bool,Bool) -> Bool
xorPair (v1,v2) = xor' v1 v2 

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = map xorPair (zip list1 list2)

inputList = [True,True,False,False,True,False,False,True]
encodingList = [False,True,False,True,True,True,False,True] 
encrypted = inputList `xor` encodingList
decoded = encrypted `xor` encodingList

type Bits = [Bool]

intToBits' :: Int -> Bits
intToBits' 0 = [False]
intToBits' 1 = [True]
intToBits' n = if (remainder == 0)
               then False : (intToBits' nextVal)
               else True : (intToBits' nextVal) 
 where remainder = n `mod` 2
       nextVal = n `div` 2
	   
maxBits :: Int
maxBits = length (intToBits' maxBound) 

intToBits :: Int -> Bits
intToBits n = leadingFalses ++ reversedBits
   where reversedBits = reverse  (intToBits' n)
         missingBits = maxBits - (length reversedBits)
         leadingFalses = take missingBits (cycle [False])         

charToBits :: Char -> Bits
charToBits char = intToBits (fromEnum char) 

bitsToInt :: Bits -> Int
bitsToInt bits = sum (map (\x -> 2^(snd x)) trueLocations)
 where size = (length bits)
       indices = [size-1,size-2 .. 0]
       trueLocations = filter (\x ->
                                ((fst x) == True))
                       (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar bits = toEnum (bitsToInt bits)

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell" 

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =  map (\pair ->
                                 (fst pair) `xor` (snd pair))
                           (zip padBits plaintextBits)
 where padBits =  map charToBits pad
       plaintextBits =  map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
 where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

class Cipher a where
   encode :: a -> String -> String
   decode :: a -> String -> String

data Rot = Rot

instance Cipher Rot where
   encode Rot text = rotEncoder text
   decode Rot text = rotDecoder text

data XOROneTimePad = XOROTP String

instance Cipher XOROneTimePad where
   encode (XOROTP pad) text = applyOTP pad text
   decode (XOROTP pad) text = applyOTP pad text

myOTP :: XOROneTimePad
myOTP = XOROTP (cycle [minBound .. maxBound])


prng :: Int -> Int -> Int -> Int -> Int
prng a b maxNumber seed = (a*seed + b) `mod` maxNumber

examplePRNG :: Int -> Int
examplePRNG  = prng 1337 7 100
