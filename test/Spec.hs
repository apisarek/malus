module Main where -- Cabal won't allow Spec module, it needs Main to start tests

import Data.List
import Data.Char
import Test.Hspec
import Test.QuickCheck
import Preprocessing (preprocessEmail, vectorizeMail, commonWords)


createMail :: [Char] -> [Char]
createMail s = "Subject: " ++ s ++ " Thanks."

allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted xs = foldr (\a f b -> (a >= b) && f a) (const True) (tail xs) (head xs)

main :: IO ()
main = hspec $ do

    describe "preprocessEmail" $ do
        it "removes the first word" $ do
            preprocessEmail "Subject:" `shouldBe` []
            preprocessEmail "Subject: test" `shouldBe` ["test"]

        it "lowercases all words" $ do
            preprocessEmail "Subject: Johny Bravo and his girl"
                `shouldBe` ["johny", "bravo", "and", "his", "girl"]

        it "removes numbers" $ do
            preprocessEmail "Subject: 1234" `shouldBe` []
            preprocessEmail "Subject: 1234 135 123" `shouldBe` []
            preprocessEmail "Subject: 1234 johny 135 bravo 123"
                `shouldBe` ["johny", "bravo"]

        it "removes punctuation marks" $ do
            preprocessEmail "Subject: wow, johny bravo!"
                `shouldBe` ["wow", "johny", "bravo"]
            preprocessEmail "Subject: 1? That's not enough."
                `shouldBe` ["thats", "not", "enough"]

        it "removes short words (length <=2)" $ do
            preprocessEmail "Subject: a bc" `shouldBe` []
            preprocessEmail "Subject: johny is an important man"
                `shouldBe` ["johny", "important", "man"]

        it "returns [] on empty string or string with whitespace only" $ do
            preprocessEmail "" `shouldBe` []
            preprocessEmail "        " `shouldBe` []

        it "works like tail on already preprocessed" $
            property $
                \mail -> (preprocessEmail . unwords . preprocessEmail $
                                (createMail $ mail :: String)
                            ) == (tail . preprocessEmail $ createMail mail)

        it "returns lowercased words" $
            property $
                \mail -> all (\word -> all isLower word) $
                preprocessEmail $ show (createMail $ mail :: String)

        it "returns words longer than 2" $
            property $
                \mail -> all (\word -> length word > 2) $
                preprocessEmail $ show (createMail $ mail :: String)


    describe "vectorizeMail" $ do
        -- example dictionary used for vectorization
        let dict = [(1, "johny"), (2, "bravo"), (3, "really"), (4, "handsome")]
        let dictLength = length dict

        it "preprocesses mail and vectorizes it to a double array" $ do
            vectorizeMail dict "Subject: johny bravo is really handsome"
                `shouldBe` [1.0, 1.0, 1.0, 1.0]
            vectorizeMail dict "Subject: james delta is quite ugly"
                `shouldBe` [0.0, 0.0, 0.0, 0.0]
            vectorizeMail dict "Subject: james bravo is quite handsome"
                `shouldBe` [0.0, 1.0, 0.0, 1.0]
            vectorizeMail dict "Subject: johny delta is really ugly"
                `shouldBe` [1.0, 0.0, 1.0, 0.0]

        it "returns double array of length equal to dictionary's length" $
            property $
                \mail -> (length $
                    vectorizeMail dict (createMail $ mail :: String)
                ) == dictLength

        it "returns double array which contains 1.0 and 0.0 only" $
            property $
                \mail -> all (\d -> d == 1.0 || d == 0.0) $
                    vectorizeMail dict (createMail $ mail :: String)


    describe "commonWords" $ do
        it "removes rare words (occurences < 10)" $ do
            let words = replicate 4 "is" ++ ["bravo"] ++ replicate 7 "johny"
            commonWords words `shouldBe` []

        it "does not return duplicates" $ do
            let words = replicate 20 "a"
            commonWords words `shouldBe` ["a"]

        it "returns unique set of words" $ do
            property $
                \words -> allDifferent $ commonWords (words :: [String])

        it "returns sorted set of words" $ do
            property $
                \words -> isSorted $ commonWords (words :: [String])
