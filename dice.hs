-- awesome dice explorer stuff

module Dice where
import Data.List

-- a Die has an integral number of sides...
type Die = Integer
-- and Dice are numbered 1 -> numSides, if you don't like that, tough!
data Dice = Dice  { numDice  :: Integer
                  , numSides :: Die }
          deriving (Show)

-- stole this idea from David Brady in his polyhedra library,
--   though it should have been easy enough to come up with on my own...
type DiceBag = [Dice]

-- i like doubles, cause they're double the precicion!!  :P
diceMean :: Dice -> Double
diceMean d = 0.5 * fromIntegral ((1 + numSides d) * numDice d)

diceVariance :: Dice -> Double
diceVariance d = fromIntegral (((numSides d) ^ 2 - 1 ) * (numDice d)) / 12

diceMean' :: DiceBag -> Double
diceMean' (d:dx)  = diceMean d + diceMean' dx
diceMean' []      = 0

-- pointless style, cause i can!
diceStdDev :: Dice -> Double
diceStdDev = sqrt . diceVariance

diceStdDev' :: DiceBag -> Double
diceStdDev' = sqrt . diceVariance'

-- because Var(x + y) == Var(x) + Var(y) if they are independant, and dice are independant
diceVariance' :: DiceBag -> Double
diceVariance' (d:ds)  = diceVariance d + diceVariance' ds
diceVariance' []      = 0

-- algo stolen from: http://mathworld.wolfram.com/Dice.html
-- and maybe some: http://en.wikipedia.org/wiki/Dice#Probability
rollNumProb :: Dice -> Integer -> Double
rollNumProb d r = let n = numDice d
                      s = numSides d
                      lk = [0..(((r - n) `div` s))]
                  in (1 / (fromIntegral s^n )) * (fromIntegral $ sum $ map (\k -> (-1)^k * (combination n k) * (combination (r - s*k - 1) (n-1))) lk)
                  -- oh and i slaved over that ^ there...  for *HOURS*!!! it wasn't easy.

rollGTNum :: Dice -> Integer -> Double
rollGTNum d r = 1 - rollLTNum d r

rollLTNum :: Dice -> Integer -> Double
rollLTNum d r
        | r < l     = 0.0
        | r >= g    = 1.0
        | otherwise = (rollNumProb d r) + rollLTNum d (r-1)
        where l = numDice d     -- least roll on nDx is n :P
              g = (numDice d) * (numSides d)

-- find if 2 dice have the same number of sides.
sidesEQ :: Dice -> Dice -> Bool
sidesEQ a b = (numSides a) == (numSides b)

--combineDice :: DiceBag -> DiceBag
--combineDice (d:dx) = total d 0 dx
--      where total d n (l:lx) =  if sidesEQ d l
--                                then total d (n + numDice l) lx
--                                else total d n lx
--            total d n []     =  

-- test stuff

d4 = Dice 1 4
d6 = Dice 1 6
d8 = Dice 1 8
d10 = Dice 1 10
d12 = Dice 1 12

l = [x,y,z,d6]
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

