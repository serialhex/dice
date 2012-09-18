-- awesome dice explorer stuff

module Main where

data Dice = Dice [(Number, Sides)]

type Number = Integer
type Sides  = Integer

diceMean :: Dice -> Rational

main = putStrLn "Hello World"