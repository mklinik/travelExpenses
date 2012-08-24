{-# LANGUAGE TemplateHaskell #-}
module Test where

import Test.QuickCheck
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import qualified Data.List as List
import qualified Data.Set as Set

import Main hiding (main)

instance Arbitrary Person where
    arbitrary = elements [minBound .. maxBound]

instance Arbitrary PayedFor where
    arbitrary = do
        payer <- arbitrary
        amount <- arbitrary
        receivers <- elements $ tail $ List.subsequences [minBound .. maxBound]
        return (Payed payer amount receivers)

case_oneReceiver = ((Owes Klaus 200 Hans) `elem` (processAll [(Payed Hans 200 [Klaus])])) @=? True
case_twoReceivers = ((Owes Klaus 100 Hans) `elem` (processAll [(Payed Hans 200 [Klaus, Hans])])) @=? True
case_threeReceivers = ((Owes Klaus 100 Hans) `elem` (processAll [(Payed Hans 300 [Klaus, Hans, Elke])])) @=? True
case_fourReceivers = ((Owes Klaus 50 Hans) `elem` (processAll [(Payed Hans 200 [Klaus, Hans, Elke, Erna])])) @=? True
case_everythingEven = [] @=? (processAll
  [ (Payed Hans 100 [Klaus])
  , (Payed Klaus 100 [Erna])
  , (Payed Erna 100 [Elke])
  , (Payed Elke 100 [Hans])
  ])

case_negativeAmount = Set.fromList expected @=? Set.fromList result
  where result = processAll [ (Payed Hans (-30) [Klaus, Erna, Elke]) ]
        expected =
          [ (Owes Hans 10 Klaus)
          , (Owes Hans 10 Erna)
          , (Owes Hans 10 Elke)
          ]

case_negativeAndPositiveAmount = Set.fromList expected @=? Set.fromList result
  where result = processAll
          [ Payed Hans (-30) [Klaus, Erna, Elke]
          , Payed Erna 10 [Hans]
          ]
        expected =
          [ Owes Hans 10 Klaus
          , Owes Hans 10 Elke
          , Owes Hans 20 Erna
          ]

case_negativeAndPositiveAmount2 = Set.fromList expected @=? Set.fromList result
  where result = processAll
          [ Payed Hans (-30) [Klaus, Erna, Elke]
          , Payed Hans 10 [Erna]
          ]
        expected =
          [ Owes Hans 10 Klaus
          , Owes Hans 10 Elke
          ]

-- the case
--   [ Hans owes Erna
--   , ...
--   , Hans owes Erna
--   ]
-- does not occur
prop_eachPairOfPersonsAppearsOnlyOnce expenses = length result == (Set.size $ Set.fromList $ map personsOnly result)
  where result = processAll expenses
        personsOnly (Owes p1 _ p2) = (p1, p2)

prop_allResultAmountsArePositive expenses = all (> 0) $ map amountOnly result
  where result = processAll expenses
        amountOnly (Owes _ amount _) = amount

main = $(defaultMainGenerator)
