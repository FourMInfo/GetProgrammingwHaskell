--stack exec ghci --package random-fu
-- import Data.Random
-- couldn't use because of the Monad

half :: Int -> Double
half n = fromIntegral n / 2

halve :: Int -> Int
halve n = n `div` 2

printDouble :: Int -> String
printDouble n = show (n*2)

--filter :: (a -> Bool) -> [a] -> `[a]

--head :: [a] -> a
myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

myFoldl :: (a -> a -> a) -> a -> [a] -> a
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs
  where newInit = f init x

-- author: myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- not sure why he uses b, since the function has to work on elements of same type in list


type Age = Int
type Height = Int
type Weight = Int

-- type PatientName = (String,String)
-- firstName :: PatientName -> String
-- firstName = fst
-- lastName :: PatientName -> String
-- lastName = snd

-- patientInfo ::PatientName -> Age -> Height -> String
-- patientInfo patientName age height = name ++ " " ++ ageHeight
--  where name = lastName patientName ++ ", " ++ firstName patientName
--        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

--book version
-- patientInfoV2 :: PatientName -> Int -> Int -> String
-- patientInfoV2 (fname,lname) age height = name ++ " " ++ ageHeight
--  where name = lname ++ ", " ++ fname
--        ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female deriving (Eq, Ord, Show, Read, Enum, Bounded)
data RhType = Pos | Neg deriving (Eq, Ord, Show, Read, Enum, Bounded)
data ABOType = A | B | AB | O deriving (Eq, Ord, Show, Read, Enum, Bounded)
data BloodType = BloodType ABOType RhType deriving (Eq, Ord, Show, Read, Bounded)

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh
showSex :: Sex -> String
showSex Male = "M"
showSex Female = "F"

-- patientInfo :: PatientName -> Age -> Height -> Sex -> BloodType -> String
-- patientInfo (fname,lname) age height sex bloodType = name ++ " " ++ stats
--             where name = lname ++ ", " ++ fname
--                   stats = "(" ++ show age ++ " yrs, " ++ show height ++ " in, " ++ showSex sex ++ ", type " ++ showBloodType bloodType ++ ")"

-- patientInfo ("John", "Doe") 43 74 Male (BloodType A Pos)
-- "Doe, John (43 yrs, 74 in, M, type A+)"

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
                 | NameWithMiddle FirstName MiddleName LastName
            deriving (Eq,Show,Read)

    
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

showNameR :: Name -> String
showNameR (Name f l) = l ++ ", " ++ f
showNameR (NameWithMiddle f m l) = l ++ ", " ++ f ++ " " ++ m

type PatientName = Name

-- patientInfo :: PatientName -> Age -> Height -> Sex -> BloodType -> String
-- patientInfo patientName age height sex bloodType = name ++ " " ++ stats
--              where name = showNameR patientName
--                    stats = ": " ++ show age ++ " yrs, " ++ show height ++ " in, " ++ showSex sex ++ ", type " ++ showBloodType bloodType

-- patientInfo (Name "John" "Doe") 43 74 Male (BloodType A Pos)
-- "Doe, John : 43 yrs, 74 in, M, type A+"
-- patientInfo (NameWithMiddle "John" "M." "Doe") 43 74 Male (BloodType A Pos)
-- "Doe, John M. : 43 yrs, 74 in, M, type A+"

-- data Patient = Patient Name Age Height Weight Sex BloodType
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient {
                     name   = Name "John" "Doe"
                   , age    = 30 
                   , height = 74 
                   , weight = 200 
                   , sex    = Male 
                   , bloodType = BloodType AB Pos
}

janeDoe :: Patient
janeDoe = Patient {
                    name   = Name "John" "Doe"
                  , age    = 35
                  , height = 64 
                  , weight = 125 
                  , sex    = Female 
                  , bloodType = BloodType A Pos
}

patientSummary :: Patient -> String
patientSummary patient = line ++ "\n" 
                        ++ "Patient Name: " ++ showNameR (name patient) ++ "\n" 
                        ++ "Sex: " ++ showSex (sex patient) ++ "\n"
                        ++ "Age: " ++ show (age patient) ++ "\n"
                        ++ "Height: " ++ show (height patient) ++ " in.\n"
                        ++ "Weight: " ++ show (weight patient) ++ " lb.\n"
                        ++ "BloodType: " ++ showBloodType (bloodType patient)  ++ "\n"
                        ++ line ++ "\n"    
                        where line = "**************\n"

patientSummary2 :: Patient -> String
patientSummary2 patient = line ++ "\n" 
                        ++ "Patient Name: " ++ showNameR (name patient) ++ "\n" 
                        ++ "Sex: " ++ show (sex patient) ++ "\n"
                        ++ "Age: " ++ show (age patient) ++ "\n"
                        ++ "Height: " ++ show (height patient) ++ " in.\n"
                        ++ "Weight: " ++ show (weight patient) ++ " lb.\n"
                        ++ "BloodType: " ++ show (bloodType patient)  ++ "\n"
                        ++ line ++ "\n"    
                        where line = "**************\n"

patientCanDonateTo :: Patient -> Patient -> Bool
patientCanDonateTo patient1 patient2 = canDonateTo (bloodType patient1) (bloodType patient2)

class Describable a where
    describe :: a -> String

data Icecream = Chocolate | Vanilla deriving (Show, Eq, Ord)

inc :: Int -> Int
inc x = x + 1

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
                then minBound
                else succ n

class (Eq a, Enum a) => Die a where
  roll :: Int -> a

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum,Eq,Show)
-- instance Show SixSidedDie where
--   show S1 = "1"
--   show S2 = "2"
--   show S3 = "3"
--   show S4 = "4"
--   show S5 = "5"
--   show S6 = "6"
-- instance Eq SixSidedDie where
--   (==) S1 S1 = True
--   (==) S2 S2 = True
--   (==) S3 S3 = True
--   (==) S4 S4 = True
--   (==) S5 S5 = True
--   (==) S6 S6 = True
--   (==) _  _  = False
-- instance Eq SixSidedDie where
--   (==) num1 num2 = fromEnum num1 == fromEnum num2

-- instance Ord SixSidedDie where
--   compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

instance Die SixSidedDie where
  roll n = toEnum (n `mod` 6)
