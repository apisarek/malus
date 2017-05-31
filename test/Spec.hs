module Main where -- Cabal won't allow Spec module, it needs Main to start tests

import Test.Hspec
import Test.QuickCheck
import Preprocessing (checkEmail)

main :: IO ()
main = hspec $ do
    describe "checkSpamMails" $ do
        --Unit tests examples
        it "is a spam mail" $ do
            checkEmail "aaabbbcccddd" `shouldBe` True

        it "is not a spam mail" $ do
            checkEmail "aaa" `shouldBe` False

        --QuickCheck example
        it "reverse of concatenation is equal to inversed concatenation of reversed" $ property $
            \xs ys -> reverse ((xs :: [Int]) ++ (ys :: [Int])) == reverse ys ++ reverse xs
