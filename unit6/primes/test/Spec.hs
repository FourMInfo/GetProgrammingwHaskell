import Test.QuickCheck
import Primes
import Data.Maybe

prop_validPrimesOnly val = if val < 0 || val >= (length primes)
                       then result == Nothing
                       else isJust result
  where result = isPrime val


prop_primesArePrime val = if result == (Just True)
                          then (length divisors) == 0
                          else True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val = if result == (Just False)
                                then (length divisors) > 0
                                else True
  where result = isPrime val
        divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000} prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000} prop_nonPrimesAreComposite