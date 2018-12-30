-- cup flOz = \message -> message flOz
cup flOz message = message flOz
-- getOz aCup = aCup (\flOz -> flOz)
getOz aCup = aCup id

coffeeCup = cup 12

drink aCup flOz = if remain > 0 
                    then cup remain
                    else cup 0
                where remain = getOz aCup - flOz

isEmpty aCup = getOz aCup == 0

-- afterManySips = foldl drink coffeeCup [1,1,1,1,1]
