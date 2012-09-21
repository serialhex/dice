-- awesome dice explorer stuff

module Main where
import Data.List

data Dice = Dice  { numDice  :: Integer
                  , numSides :: Integer }
          deriving (Show)

-- i like doubles, cause they're double the precicion!!  :P
diceMean :: Dice -> Double
diceMean d = 0.5 * fromIntegral ((1 + numSides d) * numDice d)

diceMean' :: [Dice] -> Double
diceMean' (d:dx)  = diceMean d + diceMean' dx
diceMean' []      = 0

-- pointless style, cause i can!
diceStdDev :: Dice -> Double
diceStdDev = sqrt . diceVariance

diceStdDev' :: [Dice] -> Double
diceStdDev' = sqrt . diceVariance'

diceVariance :: Dice -> Double
diceVariance d = fromIntegral (((numSides d) ^ 2 - 1 ) * (numDice d)) / 12

diceVariance' :: [Dice] -> Double
diceVariance' (d:ds)  = diceVariance d + diceVariance' ds
diceVariance' []      = 0

-- algo stolen from: http://mathworld.wolfram.com/Dice.html
-- and maybe some: http://en.wikipedia.org/wiki/Dice#Probability
rollNumProb :: Dice -> Integer -> Double
rollNumProb d p = let n = numDice d
                      s = numSides d
                      lk = [0..(((p - n) `div` s))]
                  in (1 / (fromIntegral s^n )) * (fromIntegral $ sum $ map (\k -> (-1)^k * (combination n k) * (combination (p - s*k - 1) (n-1))) lk)

rollLTNum d p = undefined

-- test stuff

l = [x,y,z,ad6]
ad6 = Dice 1 6
td6 = Dice 2 6
x = Dice 5 6
y = Dice 6 12
z = Dice 7 20

--main = putStrLn "Hello World"




-- bleh

-- eq found at: http://en.wikipedia.org/wiki/Combination#Example_of_counting_combinations
-- the use of rationals is there to be extra-speshul kewl!
combination :: Integral a => a -> a -> a
combination n k = round $ product $ zipWith (/) ln lk
                where rn = toRational n; rk = toRational k
                      ln = reverse [(rn-(rk-1))..rn]
                      lk = [1..rk]

