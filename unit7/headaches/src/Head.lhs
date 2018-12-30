
[Intro]



----Head, Partial Functions and Errors




>module Head where


One of the very first functions we where introduced to was head. The head function gives us the first element in the list, if there is one. Initially head seems like an incredibly useful function. Many recursive functions we write in Haskell use lists, and accessing the firs item in a list is acommon requirement.

There is one very big issue with head. When we call head on an empty list we get an error:

GHCi> head [1]
1
GHCi> head []
*** Exception: Prelude.head: empty list

In most programming languages this would be too surprising. In Haskell this is a big problem.

One of the key benefits of using Haskell is that our programs are safer and more predictable. However nothing about the head function, nor its type signature, given us any indication that it could suddenly blow up. By this point in the book you have seen first hand that if a Haskell program compiles, then it likely runs as expected. But head violates this rule by making it easy to write code that compiles, but then causes an error at run time.

For example suppose we naively implemented a recursive myTake function using head and tail:

->myTake :: Int -> [a] -> [a]
->myTake 0 _ = []
->myTake n xs = (head xs) : myTake (n-1) (tail xs)

Let's compile this code, only this time we'll set a compiler flag to warn us of any errors. We can do this by using the -Wall flag. This can be done in stack by adding -Wall to the ghc-options value in the extecutable section of the .cabal file:

executable headaches-exe
  hs-source-dirs:      app
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall -fforce-recomp
  build-depends:       base
                     , headaches
  default-language:    Haskell2010

Now if we build our project (or just compile our code) we'll get no complaints from the compiler. However it is trivial to see that this code produces an error:


*Main Head> myTake 2 [1,2,3] :: [Int]
[1,2]
*Main Head> myTake 4 [1,2,3] :: [Int]
[1,2,3,*** Exception: Prelude.head: empty list

Imagine if this code was running and processing requests from a user. This kind of failure would be very frustrating, especially given that we're using Haskell. 

To understand why head is so dangerous, let's ompare this to the exact same version using pattern matching:

>myTakePM :: Int -> [a] -> [a]
>myTakePM 0 _ = []
>myTakePM n (x:xs) = x : (myTakePM (n-1) xs) 

This code is identicial in behavior to myTake, but we compile with -Wall we get a very helpful error:

Pattern match(es) are non-exhaustive
    In an equation for ‘myTakePM’:
        Patterns not matched: p [] where p is not one of {0}

This tells us that our function does not have a patter for the case of the empty list! Even though this is identical to the code using head, GHC is able to warn us about this.

--Head and partial functions

The head function is an example of a class of functions called "partial functions". In lesson one we discussed how every function must take an argument and return a result. Paritial functions don't violate this rule, but they have one very significant failling. Partial functions are not defined on all inputs.The head function is undefined on the empty list.

When you think about, nearly all errors in software are the result of partial functions. Our program receives some input we didn't expect, and the program has no way of dealing with it. Throwing an error is an obvious solution to this problem. Throwing errors in Haskell is very simple, we just use the error function. Here is myHead with an error:

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:_) = x

In Haskell throwing errors is considered bad form. This is because, as we saw with myTake, it is easy to introduce bugs into code that the compiler cannot check. In practice you should never use head, and instead use pattern matching. If you replace any instance of using head and tail in our code with pattern matching then you the compiler can warn you of errors.


The real question is, what do we do about partial functions? Ideally we want a way to transform our partial function into one that works on all values. Another common partial function is (/) which is undefined for 0. However, Haskell avoids throwing an error in this case by providing a different solution:

GHCi>  2 / 0
Infinity

This is a nice solution to the problem of dividing by zero, but solutions like this only exist for a few specific case. 


--Maybe

It turns out we've already seen on of the most useful ways to handle partial function: Maybe. Many of the examples of Maybe we've used have been in the case where there would be a Null value in othe rlanguages. However, Maybe is a reasonable way to transform any partial function into a complete function. Here is our code for maybeHead:


>maybeHead :: [a] -> Maybe a
>maybeHead [] = Nothing
>maybeHead (x:_) = Just x

Here is an example of using our maybeHead function, as well as using <$> to operate on the values it produced:

*Main Head> maybeHead [1]
Just 1
*Main Head> maybeHead []
Nothing
*Main Head> (+2) <$> maybeHead [1]
Just 3
*Main Head> (+2) <$> maybeHead []
Nothing

We've spend a lot of time in this book talking about the power of Maybe, but it does have one major limitation. As we write more sophisticated programs the Nothing result becomes harder to interprut. Recall in our capstone we had an isPrime function. We made this function of type Int -> Maybe Bool because we wanted handle our edge cases. The key issue is that we wanted a False value for isPrime to mean that a number is composite. But there were two problems. Numbers like 0,1 are neither composite nor prime. Additioanlly our isPrime function had an upper bound, and we didn't want to return false just because a value was too expensive to compute. Here's a simplified version of isPrime:

>primes :: [Int]
>primes = [2,3,5,7]

>maxN :: Int
>maxN = 10

->isPrime :: Int -> Maybe Bool
->isPrime n
->   | n < 2 = Nothing
->   | n > maxN = Nothing
->   | otherwise = Just (n `elem` primes)
-> 

Now imagine you're using this function in yoru own software. When you call isPrime 9997 and get Nothing as a result, what in the world does this actually mean? The nice thing about errors is you get an error message. While Maybe does give us lots of safety, unless Nothing has an obvious interprutation it's not very useful. Fortunately Haskell has another type, very similiar to Maybe that allows us to create much more expressive errors, while remaining safe.


----Introducting the Either type

The type we'll be looking at is called Either. Though only a bit more complicated the Maybe, it's definition can certainly be confusing. Here is the definition of Either:

->data Either a b = Left a | Right b

Either has two confusingly named data constructors: Left and Right. For the case of handling errors we can consider the Left constructor as the case of having and error, and the Right constructor for when things go as place. In practice the Right constructor works exactly like Just for Maybe. The key difference between the two is that Left allows us to have more information than Nothing. Also notice that Either takes two type parameters. This allows us to have a type for sending error message and a type for our actual data. To demonstrate here's an example of making a safer head function with either:


>eitherHead :: [a] -> Either String a
>eitherHead [] = Left "There is no head because the list is empty"
>eitherHead (x:xs) = Right x

Notice that the Left constructor takes a String while the Right constructor returns whatever value our list is. Here are some example lists we can test on:

>intExample :: [Int]
>intExample = [1,2,3]

>intExampleEmpty :: [Int]
>intExampleEmpty = []

>charExample :: [Char]
>charExample = "cat" 

>charExampleEmpty :: [Char]
>charExampleEmpty = "" 

In GHCi we can see how Either works, as well as what types we get back:


GHCi> eitherHead intExample
Right 1
GHCi> eitherHead intExampleEmpty 
Left "There is no head because the list is empty"
GHCi> eitherHead charExample
Right 'c'
GHCi> eitherHead charExampleEmpty 
Left "There is no head because the list is empty"
*Main Head> 

The Either type is also a member of Monad (and thus Functor and Applicative as well). Here is a simple example of using <$> to increment the head of our intExample:


GHCi> (+ 1) <$> (eitherHead intExample)
Right 2
GHCi> (+ 1) <$> (eitherHead intExampleEmpty)
Left "There is no head because the list is empty"

The Either type combines the safety of Maybe, with clarity that error messages provide us. 

---Now let's build a prime checker

To demonstrate working with Either let's build a basic command line tool to check whether or not a number is prime. We'll keep our isPrime function very minimal, focusing on Either...


The nice thing about Either is we don't have to stick to a single error message. We can have as many as we'd like. Our improved isPrime function will let us now whether a value is not a valid candidate for pimality checking, or if the number is too large.

->isPrime :: Int -> Either String Bool
->isPrime n
->   | n < 2 = Left "Numbers less than 2 are not candidates for primes"
->   | n > maxN = Left "Value exceeds limits of prime checker"
->   | otherwise = Right (n `elem` primes)

Here are a few tests of this function in GHCi:

*Main Head> isPrime 5
Right True
*Main Head> isPrime 6
Right False
*Main Head> isPrime 100
Left "Value exceeds limits of prime checker"
*Main Head> isPrime (-29)
Left "Numbers less than 2 are not candidates for primes"
*Main Head> 

So far we haven't really taken advantage of the fact that Either takes two types, we've used excclusively String for the Left constructor. In most programming language we can represent errors using a class. This make it easier to model specific types of errors. Either allows us to do this as wel. Let's start by making our errors into a type of their own:

>data PrimeError = TooLarge | InvalidValueo

Now we can make this an instance of Show so that we can easily print out these errors

>instance Show PrimeError where
>   show TooLarge = "Value exceed max bound"
>   show InvalidValue = "Value is not a valid candidate for prime checking"

With our new PrimeError type we can refactor our isPrime function to show these errors off.

>isPrime :: Int -> Either PrimeError Bool
>isPrime n
>   | n < 2 = Left InvalidValue
>   | n > maxN = Left TooLarge
>   | otherwise = Right (n `elem` primes)
> 

This makes our code much more readable. Additionally now we have an easily reusuable data type that will work with our errors. Here are some examples of our new function in GHCi

GHCi> isPrime 99
Left Value exceed max bound
GHCi> isPrime 0
Left Value is not a valid candidate for prime checking

Next we'll create a display result function that will convert our Either response into a String:

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It's prime"
displayResult (Right False) = "It's composite"
displayResult (Left primeError) = show primeError

Finally we can put together a simple main IO action that read


main :: IO ()
main = do
  print "Enter a number to test for primality:"
  n <- (read <$> getLine)
  let result = isPrime n
  print (displayResult result)



$ stack exec headaches-exe
"Enter a number to test for primality:"
6
"It's composite"
(general)work-lappy:headaches willkurt$ stack exec headaches-exe
"Enter a number to test for primality:"
5
"It's prime"
(general)work-lappy:headaches willkurt$ stack exec headaches-exe
"Enter a number to test for primality:"
213
"Value exceed max bound"
(general)work-lappy:headaches willkurt$ stack exec headaches-exe
"Enter a number to test for primality:"
0
"Value is not a valid candidate for prime checking"
(general)work-lappy:headaches willkurt$


With our PrimeError type we were able to replicate more sophosticated ways of modeling errors in OOP programming langauges. The great thing about Either is because the Left constructor can be any type there no limit to how expressive we can be. If we wanted to we could return a function! 

