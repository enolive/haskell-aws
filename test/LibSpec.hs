{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module LibSpec
  ( spec,
  )
where

import Aws.Lambda (Context (..))
import Data.Foldable (for_)
import qualified Data.HashMap.Lazy as HashMap
import Lib
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

data Case
  = Case
      { input :: Int,
        expected :: String
      }

cases :: [Case]
cases =
  [ Case {input = 1, expected = "1"},
    Case {input = 2, expected = "2"},
    Case {input = 4, expected = "4"},
    Case {input = 3, expected = "Fizz"},
    Case {input = 6, expected = "Fizz"},
    Case {input = 5, expected = "Buzz"},
    Case {input = 10, expected = "Buzz"},
    Case {input = 15, expected = "Fizz-Buzz"},
    Case {input = 30, expected = "Fizz-Buzz"}
  ]

spec :: Spec
spec =
  describe "Handler for Fizz-Buzz" $ do
    context "single" $ for_ cases test
    context "sequence" $ do
      it "empty" $ sequenceUpTo 0 `shouldBe` []
      it "simple" $ sequenceUpTo 3 `shouldBe` ["1", "2", "Fizz"]
      it "full" $ sequenceUpTo 15 `shouldBe` ["1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz", "11", "Fizz", "13", "14", "Fizz-Buzz"]
    context "extract limit" $ do
      it "works" $
        extractLimit (HashMap.fromList [("input", "0")]) `shouldBe` Just 0
      it "fails when input is missing" $
        extractLimit HashMap.empty `shouldBe` Nothing
      it "fails when input is not a number" $
        extractLimit (HashMap.fromList [("input", "BAM!")]) `shouldBe` Nothing
    context "validate limit" $ do
      it "works" $ property $ forAll (choose (0, maxLimit)) $
        \x -> validateLimit x `shouldBe` Just x
      it "fails for bigger numbers" $ forAll (choose (maxLimit + 1, maxBound)) $
        \x -> validateLimit x `shouldBe` Nothing
      it "fails for small numbers" $ forAll (choose (minBound, -1)) $
        \x -> validateLimit x `shouldBe` Nothing
    context "handler" $ do
      it "works for valid input" $
        handler (eventWithParams [("input", "1")]) testContext `shouldReturn` Right Response { statusCode = 200, body = "[\"1\"]"}
      it "returns nothing for invalid input" $
        handler (eventWithParams [("input", "BAM")]) testContext `shouldReturn` Right Response {statusCode = 200, body = ""}
  where
    test Case {..} = it ("converts " <> show input <> " to " <> expected) $ single input `shouldBe` expected

event :: Event
event = Event {resource = "test", pathParameters = HashMap.empty}

eventWithParams :: [(String, String)] -> Event
eventWithParams params = event {pathParameters = HashMap.fromList params}

testContext :: Context
testContext =
  Context
    { functionName = "test",
      memoryLimitInMb = 1,
      functionVersion = "1",
      invokedFunctionArn = "1",
      awsRequestId = "42",
      xrayTraceId = "1",
      logStreamName = "test",
      logGroupName = "test",
      deadline = 42
    }
