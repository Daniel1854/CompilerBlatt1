module Lib
    ( someFunc
    , square
    , ggt
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

square :: Num a => a -> a
square a = a^2 

ggt :: Integer -> Integer -> Integer 
ggt a b = if b == 0
    then abs a 
    else ggt b (a `mod` b)

     
