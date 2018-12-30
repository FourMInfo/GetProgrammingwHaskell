import Data.List

ifEven myFunction n = if even n
    then myFunction n
    else n

compareLastNames name1 name2
    | lastName1 > lastName2 = GT
    | lastName1 < lastName2 = LT
    | otherwise = EQ
    where lastName1 = snd name1
          lastName2 = snd name2

compareLastNames2 name1 name2
    | lastName1 > lastName2 = GT
    | lastName1 < lastName2 = LT
    | otherwise = if firstName1 > firstName2  
                   then GT
                   else if firstName1 < firstName2
                    then LT
                    else EQ
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2

names = [("Ian", "Curtis"),
          ("Bernard","Sumner"),
          ("Peter", "Hook"),
          ("Stephen","Morris"),
          ("Joseph", "Morris")]


sfOffice name = if lastName < "L"
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
            where lastName = snd name
                  nameText = fst name ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = fst name ++ " " ++ snd name

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

dcOffice name = nameText ++ ": PO Box 789 - Washington, DC, 10013"
    where nameText = fst name ++ " " ++ snd name ++ " Esq."

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "dc" -> dcOffice
    _ -> \name -> fst name ++ " " ++ snd name
          
addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location

compareLastNames3 name1 name2 =
    if result == EQ
        then compare firstName1 firstName2 
        else result
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2
          result = compare lastName1 lastName2

addressLetterV2 = flip addressLetter
addressLetterNY =  addressLetterV2 "ny"
addressLetterSF =  addressLetterV2 "sf"
addressLetterRENO =  addressLetterV2 "reno"
addressLetterDC =  addressLetterV2 "dc"

-- addressLetterNY ("Bob", "Smith")
-- addressLetterSF ("Bob", "Smith")
-- addressLetterDC ("Bob", "Smith")
-- addressLetterRENO  ("Bob", "Smith")

subtract2 = flip (-) 2
