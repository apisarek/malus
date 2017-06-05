module Main where -- Cabal won't allow Spec module, it needs Main to start tests

import Test.Hspec
import Test.QuickCheck
import Preprocessing (preprocessEmail, vectorizeMail, commonWords)


createMail :: [Char] -> [Char]
createMail s = "Subject: " ++ s ++ " Thanks."

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


    describe "commonWords" $ do
        it "removes rare words (occurences < 10)" $ do
            let words = replicate 4 "is" ++ ["bravo"] ++ replicate 7 "johny"
            commonWords words `shouldBe` []

        it "does not return duplicates" $ do
            let words = replicate 20 "a"
            commonWords words `shouldBe` ["a"]
