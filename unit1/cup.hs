cup oz = \message -> message oz

coffeeCup = cup 12
getOz aCup = aCup (\oz -> oz)

drink aCup ozDrank = if ozDiff >= 0
                     then cup (oz - ozDrank)
                     else cup 0
 where oz = getOz aCup
       ozDiff = oz - ozDrank

isEmpty aCup = (getOz aCup) == 0

afterManySips = foldl drink coffeeCup [1,1,1,1,1]
