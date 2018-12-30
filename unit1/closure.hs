genIfEvenX x = \f -> if even x
                then f x
                else x

--genIfEvenX 2 (\x -> x*2) == 4
--genIfEvenX 3 (\x -> x*2) == 3

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

genHostRequestBuilder host =  \apiKey resource id -> getRequestURL host apiKey resource id

-- exampleUrlBuilder = genHostRequestBuilder "http://example.com"
-- exampleUrlBuilder "1337hAsk3ll" "book" "1234"

genApiRequestBuilder hostBuilder apiKey = \resource id ->
    hostBuilder apiKey resource id

--exampleAPIBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll"
--exampleAPIBuilder "book" "1234"

genResourceBuilder  apiBuilder resource = \id -> apiBuilder resource id

-- exampleResourceBuilder = genResourceBuilder exampleAPIBuilder "book"
-- exampleResourceBuilder "1234"

-- exampleURLBuilder = getRequestURL "http://example.com"
-- exampleAPIBuilder = exampleURLBuilder "1337hAsk3ll"
-- exampleResourceBuilder = exampleAPIBuilder "book"
-- exampleResourceBuilder "1234"

exampleBuilder = getRequestURL "http://example.com" "1337hAsk3ll" "books"
-- exampleBuilder "1234"

ifEven myFunction n = if even n
    then myFunction n
    else n

ifEvenInc = ifEven (+ 1)
ifEvenDouble = ifEven (* 2)
ifEvenSquare = ifEven (^ 2)

binaryPartialApplication binaryFunction x = (\y -> binaryFunction x y)

-- ifEvenInc = binaryPartialApplication ifEven (+1)