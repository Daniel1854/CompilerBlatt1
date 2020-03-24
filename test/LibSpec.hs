{-# LANGUAGE ScopedTypeVariables #-}
module LibSpec (spec) where

import Lib (square, ggt)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
    describe "square" $ do
         it "calculates the square of 5.3" $
             square 5.3 `shouldBe` 28.09
         it "calculates the square of an arbitrary integer" $
             property $ \(n :: Integer) -> square n == n * n
         it "calculates the square of an arbitrary double" $
             property $ \(n :: Double) -> square n == n * n
    describe "ggt" $ do
         it "calculates the ggt of 20 and 42" $
             ggt 20 42 `shouldBe` 2
         it "calculates the ggt of 15 and 25" $
             ggt 15 25 `shouldBe` 5
         it "calculates the same as Haskells gcd" $
             property $ \a b -> ggt a b == gcd a b
