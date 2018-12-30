{-# LANGUAGE OverloadedStrings #-}

--Goal, write a script to extract all Author title combos and write
--them to seperate file

import Data.List -- this is NOT needed for the capstone
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T

--needed
import GHC.Int



--Where we want to end up

type Tag = BC.ByteString
type SubfieldCode = BC.ByteString


---Rethink this so it's book data

--What we really care about is Book data

type Author = T.Text
type Title = T.Text
data Book = Book Author Title


--unfortunately marc records are dramatically more complex than this





type MarcRecordRaw = BC.ByteString
type MarcLeaderRaw = BC.ByteString

numToBC :: Int -> BC.ByteString
numToBC n = BC.pack charString
  where char = toEnum n
        charString = [char]

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

leaderLength :: Int64
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = BC.take leaderLength record


getRecordLength :: MarcLeaderRaw -> Int64
getRecordLength leader = (read . BC.unpack)  (BC.take 5 leader)



--operate on the Raw
--This is the location of the end of the leader + directory
getBaseAddress :: MarcLeaderRaw -> Int64
getBaseAddress leader = (read . BC.unpack) (BC.take 5 remainder)
  where remainder = BC.drop 12 leader

getDirectoryLength :: MarcLeaderRaw -> Int64
getDirectoryLength leader = (getBaseAddress leader) - (leaderLength + 1)


--Next get the direcotry
type MarcDirectoryRaw = BC.ByteString

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = BC.take directoryLength afterLeader
    where directoryLength = getDirectoryLength record
          afterLeader = BC.drop leaderLength record



--Split the directory up in to a List of direcotry tiems

type MarcDirectoryEntryRaw = BC.ByteString

dirEntryLength :: Int64
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == BC.empty
                           then []
                           else nextEntry : (splitDirectory restEntries)
  where nextEntry = BC.take dirEntryLength directory
        restEntries = BC.drop dirEntryLength directory

---Now we want to actual model Entries as a data type. We'll use
---record syntax so it's easy to work with these

data FieldMetadata = FieldMetadata { tag         :: BC.ByteString
                   , fieldLength :: Int64
                   , fieldStart  :: Int64 } deriving Show

--note: you need to explain BC.splitAt

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata theTag theLength theStart
  where (theTag,rest) = BC.splitAt 3 entry
        (rawLength,rawStart) = BC.splitAt 4 rest
        theLength = (read . BC.unpack) rawLength
        theStart = (read . BC.unpack) rawStart

getFieldMetadata ::  [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetadata rawEntries

type FieldRaw = BC.ByteString

getRawField :: MarcRecordRaw -> FieldMetadata -> FieldRaw
getRawField record fieldMetadata = BC.take (fieldLength fieldMetadata) baseAtEntry
  where recordLength = getRecordLength record
        baseAddress = getBaseAddress record
        baseRecord = BC.drop baseAddress record
        baseAtEntry = BC.drop (fieldStart fieldMetadata) baseRecord
  

--okay we just need the Title and the Author.
--The title is just field 245 subfield 1

--rather than build a type to model subfields and all that
--we'll just go after what we want

lookupFieldMetadata :: BC.ByteString -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if (length results) < 1
                                 then Nothing
                                 else Just (head results)
                                      
  where metadata = (getFieldMetadata . splitDirectory . getDirectory) record
        results = filter ((== aTag) . tag) metadata



lookupSubfield :: (Maybe FieldMetadata) -> Char -> MarcRecordRaw -> Maybe BC.ByteString
lookupSubfield Nothing subfield record = Nothing
lookupSubfield (Just fieldMetadata) subfield record = if (length results) < 1
                                                         then Nothing
                                                         else Just (((BC.drop 1) . head) results)
  where rawField = getRawField record fieldMetadata
        subfields = BC.split fieldDelimiter rawField
        results = filter ((== subfield) . BC.head) subfields


lookupValue :: BC.ByteString -> Char -> MarcRecordRaw -> Maybe BC.ByteString
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
  where entryMetadata = lookupFieldMetadata aTag record



---for us later
titleTag :: BC.ByteString
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

lookupTitle :: MarcRecordRaw -> Maybe T.Text
lookupTitle record = getTitleOrNothing title
  where title = lookupValue titleTag titleSubfield record
        getTitleOrNothing Nothing = Nothing
        getTitleOrnothing (Just title) = (T.pack . BC.unpack) title

--authorTag

---Iterate through all records

nextAndRest :: BC.ByteString -> (MarcRecordRaw,BC.ByteString)
nextAndRest marcStream =  BC.splitAt recordLength marcStream
  where recordLength = getRecordLength marcStream


allRecords :: BC.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == BC.empty
                        then []
                        else next : (allRecords rest)
  where nextRestPair = nextAndRest marcStream
        next = fst nextRestPair
        rest = snd nextRestPair



main :: IO ()
main = do
  sampleData <- BC.readFile "path/to/marc/sample.mrc"
--  print (take 10 sampleData)
  let records = allRecords sampleData
  let possibleTitles = map (lookupValue titleTag titleSubfield) (take 4 records)
  print possibleTitles
  print "ah"














