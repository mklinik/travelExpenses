{-# LANGUAGE TemplateHaskell #-}
module Test where

import Test.QuickCheck
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import qualified Data.List as List

import Main hiding (main)

payedForAmount (Payed _ am _) = am


instance Arbitrary Person where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary PayedFor where
    arbitrary = do
        payer <- arbitrary
        amount <- arbitrary
        receivers <- elements $ tail $ List.subsequences [minBound .. maxBound]
        return (Payed payer amount receivers)

sumDebts = sum . (map (\(Owes _ amount _) -> amount))
sumExpenses = sum . (map (\(Payed _ amount _) -> amount))

prop_sumOfOneExpense expense = (payedForAmount expense) == (sumDebts $ pays2owes expense)
prop_sumOfLotsExpenses expenses = (sumExpenses expenses) == (sumDebts $ allDebts expenses)
case_oneReceiver = ((Owes Klaus 200 Hans) `elem` (allDebts [(Payed Hans 200 [Klaus])])) @=? True
case_twoReceivers = ((Owes Klaus 100 Hans) `elem` (allDebts [(Payed Hans 200 [Klaus, Hans])])) @=? True
case_threeReceivers = ((Owes Klaus 100 Hans) `elem` (allDebts [(Payed Hans 300 [Klaus, Hans, Elke])])) @=? True
case_fourReceivers = ((Owes Klaus 50 Hans) `elem` (allDebts [(Payed Hans 200 [Klaus, Hans, Elke, Erna])])) @=? True
case_everythingEven = (null $ processAll $ allDebts
  [ (Payed Hans 100 [Klaus])
  , (Payed Klaus 100 [Erna])
  , (Payed Erna 100 [Elke])
  , (Payed Elke 100 [Hans])
  ]) @=? True

main = $(defaultMainGenerator)
