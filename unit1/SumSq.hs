sumOfSqOrSqOfSum x y = if sumOfSquare > squareOfSum 
                        then sumOfSquare
                        else squareOfSum 
    where sumOfSquare = x^2 + y^2
          squareOfSum = (x + y)^2

lSumOfSqOrSqOfSum x y = (\sumOfSquare squareOfSum -> 
                        if sumOfSquare > squareOfSum 
                            then sumOfSquare
                            else squareOfSum) (x^2 + y^2) ((x + y)^2)


doubleDouble x = dubs*2 where dubs = x*2

ldoubleDouble x = (\dubs -> dubs * 2)(x*2)

loverwrite x = (\x -> x)((\x -> 4) ((\x -> 3) (\x -> 2)))
l2overwrite x =(\x -> (\x -> (\x -> x) 4) 3) 2

overwrite x = let x = 2
                 in
                  let x = 3
                  in
                   let x = 4
                   in
                    x


add2 y = (\x -> x + y) 3

add3 y = (\y -> (\x -> x +y ) 1) 2

-- add1 y = x + y

counter x = (\x -> (\x -> x) (x + 1)) (x + 1)
