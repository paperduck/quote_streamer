
module Tests where

--
import           Control.Exception (evaluate)
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Test.Hspec
--
import           MyLib as Lib
import           Utility
import           MyQueue as Q

main :: IO ()
main = hspec $ do

    -- Utility.hs ------------------------------------------------

    describe "splitAts" $ do
        let bs = BSLC.pack "ABCD"
        let target = [BSLC.pack "A", BSLC.pack "B", BSLC.pack "CD"]
        it "splits part of a bytestring" $
            (splitAts [1,1] bs) `shouldBe` target
        
        let bs = BSLC.pack "EFG"
        let target = [BSLC.pack "EF", BSLC.pack "G", BSLC.empty]
        it "splits an entire bytestring and nothing remains" $
            (splitAts [2,1] bs) `shouldBe` target

        let bs = BSLC.pack "WXYZ"
        let target = [BSLC.pack "WXY", BSLC.pack "Z", BSLC.empty, BSLC.empty, BSLC.empty]
        it "splits a bytestring too much and leaves some empty bytestrings" $
            (splitAts [3,3,3,3] bs) `shouldBe` target 

        let bs = BSLC.pack "XXXX"
        let target = [BSLC.pack "XXXX"]
        it "does not split a bytestring when no sizes given" $
            (splitAts [] bs) `shouldBe` target 

        let bs = BSLC.empty
        let target = [BSLC.empty, BSLC.empty]
        it "splits an empty bytestring into another empty bytestring" $
            (splitAts [3] bs) `shouldBe` target 

    describe "myDecodeInt16" $ do
        it "reads 2 bytes as an Int" $
            (myDecodeInt16 $ BSLC.pack "AB") `shouldBe` 16706

        it "errors when not 2 bytes given" $ do
            evaluate (myDecodeInt16 $ BSLC.pack "ABCDE") `shouldThrow` errorCall "myDecodeInt16: Not 2 bytes"

    describe "myDecodeInt32" $ do
        it "reads 4 bytes as an Int" $
            (myDecodeInt32 $ BSLC.pack "ABCD") `shouldBe` 1094861636

        it "errors when not 4 bytes given" $ do
            evaluate (myDecodeInt32 $ BSLC.pack "ABCDE") `shouldThrow` errorCall "myDecodeInt32: Not 4 bytes"


    -- MyQueue.hs ------------------------------------------------

    describe "MyQueue.empty" $ do
        it "has length zero" $
            (Q.size Q.empty) `shouldBe` 0
        
    describe "MyQueue.push" $ do
        it "increases size by one" $ 
            (Q.size $ Q.push 3 Q.empty) `shouldBe` 1
        it "puts the new item onto the tail" $ 
            (Q.peek $ Q.push 4 $ Q.push (5::Int) Q.empty) `shouldBe` Just 5

    describe "MyQueue.pop" $ do
        it "decreases size by one" $ do
            let (poppedQ, mVal) = Q.pop $ Q.push 6 Q.empty
            (Q.size poppedQ) `shouldBe` 0
        it "pops from the head" $ do
            let (poppedQ, mVal) = Q.pop $ Q.push 7 $ Q.push (8::Int) Q.empty
            mVal `shouldBe` Just 8
        
    describe "MyQueue.peek" $ do
        it "shows the head when queue not empty" $ 
            (Q.peek $ Q.push 1 $ Q.push 2 Q.empty) `shouldBe` Just 2
        it "returns Nothing when queue empty" $
            (Q.peek (Q.empty::Q.MyQueue Int)) `shouldBe` Nothing

    describe "MyQueue.size" $ do
        it "returns the number  of items in a non-empty queue" $
            (Q.size $ Q.push 1 $ Q.push 0 Q.empty) `shouldBe` 2
        it "says an empty queue ha size zero" $ 
            (Q.size (Q.empty::Q.MyQueue Int)) `shouldBe` 0

    describe "MyQueue.null" $ do
        it "says an empty queue is empty" $
            (Q.null (Q.empty::Q.MyQueue Int)) `shouldBe` True
        it "says a non-empty queue is not empty" $
            (Q.null $ Q.push 1 Q.empty) `shouldBe` False


