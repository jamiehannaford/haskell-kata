{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Data.List
import Data.Char

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

winsAgainst :: [DieValue] -> [DieValue] -> Bool
a `winsAgainst` b = a > b

getUnitCount :: Int -> Int -> Int
getUnitCount max units
  | units <= 1 = 0
  | units <= 3 = units - 1
  | otherwise  = max

battle :: Battlefield -> Rand StdGen Battlefield
battle bF = do
        let aC = attackers bF
            dC = defenders bF
            roll i = fmap (reverse . sort) $ replicateM i die 
        rollA <- roll $ getUnitCount 2 aC
        rollD <- roll $ getUnitCount 3 dC
        if rollA `winsAgainst` rollD
        then
          return $ Battlefield aC (dC - 1)
        else
          return $ Battlefield (aC - 1) dC

gameIsOver :: Battlefield -> Bool
gameIsOver b = if attackers b < 2 || defenders b == 0 then True else False

eval b = do
  values <- evalRandIO b
  putStrLn $ (show $ attackers values) ++ "-" ++ (show $ defenders values)

invade :: Battlefield -> Rand StdGen Battlefield
invade b = do
  let act = battle b
  result <- act
  if gameIsOver result
  then act
  else invade result
