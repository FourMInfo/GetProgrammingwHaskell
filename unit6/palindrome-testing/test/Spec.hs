import Lib
import Test.QuickCheck
import Test.QuickCheck.Instances
import Data.Char(isPunctuation)
import Data.Text as T

prop_punctuationInvariant text = (preprocess text) == (preprocess noPuncText)
  where noPuncText = T.filter (not . isPunctuation) text


main :: IO ()
main = do
  quickCheckWith stdArgs { maxSuccess = 1000}  prop_punctuationInvariant
  putStrLn "done!"
