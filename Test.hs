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

payedForAmount (PayedFor _ _ am) = am


instance Arbitrary Person where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary PayedFor where
    arbitrary = do
        payer <- arbitrary
        amount <- arbitrary
        receivers <- elements $ tail $ List.subsequences [minBound .. maxBound]
        return (PayedFor payer receivers amount)

sumDebts = sum . (map (\(Owes _ amount _) -> amount))
sumExpenses = sum . (map (\(PayedFor _ _ amount) -> amount))

prop_sumOfOneExpense expense = (payedForAmount expense) == (sumDebts $ pays2owes expense)
prop_sumOfLotsExpenses expenses = (sumExpenses expenses) == (sumDebts $ allDebts expenses)
case_oneReceiver = ((Owes Klaus 200 Hans) `elem` (allDebts [(PayedFor Hans [Klaus] 200)])) @=? True
case_twoReceivers = ((Owes Klaus 100 Hans) `elem` (allDebts [(PayedFor Hans [Klaus, Hans] 200)])) @=? True
case_threeReceivers = ((Owes Klaus 100 Hans) `elem` (allDebts [(PayedFor Hans [Klaus, Hans, Elke] 300)])) @=? True
case_fourReceivers = ((Owes Klaus 50 Hans) `elem` (allDebts [(PayedFor Hans [Klaus, Hans, Elke, Erna] 200)])) @=? True

main = $(defaultMainGenerator)
