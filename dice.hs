-- awesome dice explorer stuff

module Main where

import Data.List

-- makin sure my numbers stay in line!!!
--default (Integer, Rational, Double)

data Dice = Dice  { numDice  :: Integer
                  , numSides :: Integer }
          deriving (Show)

-- i like doubles, cause they're double the precicion!!  :P
diceMean :: [Dice] -> Double
diceMean d =  let fun (x:xs)  = ((1 + numSides x) * numDice x) + (fun xs)
                  fun []      = 0
              in (0.5 :: Double) * fromIntegral (fun d)

diceStdDev :: [Dice] -> Double
diceStdDev d =  let fun (x:xs) = fromIntegral (((numSides x) ^ 2 - 1 ) * (numDice x)) / 12 + fun xs
                    fun []     = 0
                in sqrt $ fun d

-- algo stolen from: http://mathworld.wolfram.com/Dice.html
-- and maybe some: http://en.wikipedia.org/wiki/Dice#Probability
getNum :: Dice -> Integer -> Double
getNum d n = undefined

-- test stuff

ad6 = Dice 1 6

x = Dice 2 6

y = [(Dice 3 4), (Dice 2 12)]

main = putStrLn "Hello World"




-- bleh
-- eq found at: http://en.wikipedia.org/wiki/Combination#Example_of_counting_combinations
-- the use of rationals is there to be extra-speshul kewl!
combination :: Integral a => a -> a -> a
combination n k = round $ product $ zipWith (/) ln lk
                where rn = toRational n; rk = toRational k
                      ln = reverse [(rn-(rk-1))..rn]
                      lk = [1..rk]

