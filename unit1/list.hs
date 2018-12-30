myRepeat n = cycle [n,n..]

-- myRepeat 5
-- repeat n = cycle [n]

subSeq start finish myList = reverse (drop (length myList - finish) (reverse (drop start myList)))

-- subSeq 2 5 [0..7] == [2,3,4]
-- subSeq 2 5 [1 .. 10] == [3,4,5]
-- subSeq 2 7 "a puppy" == "puppy"
-- subSeq 0 9 [0..7] == [0,1,2,3,4,5,6,7]
-- subseq start end myList = take difference (drop start myList)
--  where difference = end - start

inFirstHalf e myList = e `elem` take ((length myList) `div` 2) myList

-- inFirstHalf val myList = val 'elem' firstHalf
--  where midpoint = (length myList) 'div' 2
--  firstHalf = take midpoint myList
