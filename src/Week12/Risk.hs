{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random
import Data.List

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
  deriving (Show)

------------------------------------------------------------
-- Homework code begins here
------------------------------------------------------------
------------------------- ex 2 ----------------------------- 

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = rollDie (att,def) >>= return . newBattlefield . losses
  where
    att = attackTroops a
    def = defenseTroops d

    newBattlefield :: (Int, Int) -> Battlefield
    newBattlefield (la, ld) = Battlefield
      { attackers = a + la
      , defenders = d + ld
      }

    losses :: ([DieValue], [DieValue]) -> (Int,Int)
    losses (a, d) = tallyResults results
      where
        results = unzip $ map versus $ zip (sort a) (sort d)
        tallyResults (xs, ys) = (sum xs, sum ys)

        versus :: (DieValue, DieValue) -> (Int,Int)
        versus (a,d) | a > d = (0, -1)
        versus _ = (-1,0)

    rollDie :: (Int,Int) -> Rand StdGen ([DieValue], [DieValue])
    rollDie (a, d) = (,) <$> rollNDie  a <*> rollNDie d

rollNDie :: Int -> Rand StdGen [DieValue]
rollNDie 0 = return []
rollNDie n = (:) <$> die <*> rollNDie (n-1)

attackTroops :: Army -> Int
attackTroops a
  | a > 3 = 3
  | a > 2 = 2
  | a > 1 = 1
  | otherwise = 0

defenseTroops :: Army -> Int
defenseTroops a
  | a > 1 = 2
  | a > 0 = 1
  | otherwise = 0

-- for testing
myBattlefield :: Battlefield
myBattlefield = Battlefield 5 3

--------------------------------------------
-- implementing rollNDie equivalent
-- using (Rand StdGen) Monad instance
-- instead of Applicative instance
rollThreeDie :: Rand StdGen [DieValue]
rollThreeDie =
  die >>= \x ->
  die >>= \y -> 
  die >>= \z ->
  return [x,y,z]

rollTwoDie :: Rand StdGen [DieValue]
rollTwoDie =
  die >>= \x ->
  die >>= \y -> 
  return [x,y]

rollOneDice :: Rand StdGen [DieValue]
rollOneDice =
  die >>= \x -> 
  return [x]

------------------------- ex 3 ----------------------------- 
invade :: Battlefield -> Rand StdGen Battlefield
invade (Battlefield a d) | d==0 || a < 2 = return $ Battlefield a d
invade b = battle b >>= invade

------------------------- ex 4 ----------------------------- 
suceessfulInvasion :: Battlefield -> Rand StdGen Bool
suceessfulInvasion b = isSuccess <$> invade b
  where 
    isSuccess (Battlefield _ d)
      | d == 0 = True
      | otherwise = False 

successProb :: Battlefield -> Rand StdGen Double
successProb b = (/ 1000.0) . fromIntegral . length . filter id <$> helper 1000
  where
    helper :: Int -> Rand StdGen [Bool]
    helper 0 = return []
    helper n = (:) <$> suceessfulInvasion b <*> helper (n-1)