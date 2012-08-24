-- * A group of people makes a trip.
-- * Repeatedly: someone pays a bill for a subset of the group.
-- * How much does anyone owe to anyone else?

module Main where

import Text.Printf
import Data.List (sort)

-- configuration: edit here

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq, Enum, Bounded, Ord)

input =
    [ Hans  `Payed` 500 `for` [Klaus]
    , Klaus `Payed` 510 `for` [Hans]
    , Hans  `Payed` 10 `for` [Erna, Elke]
    ]

-- end of configuration: leave the rest as it is

everybody :: [Person]
everybody = [minBound .. maxBound]

for = ($)

-- Input: payer, amount, receivers
data PayedFor = Payed Person Rational [Person]
    deriving Show

-- Output: the first person owes an amount to the second person
data Owes = Owes Person Rational Person
    deriving (Eq, Ord)

instance Show Owes where
    show (Owes pA amount pB) =
           (show pA) ++ " owes "
        ++ (printf "%.2f" ((fromRational amount)::Double))
        ++ " to " ++ (show pB)

newtype Account = Account (Person, Rational)
  deriving (Eq, Show)

instance Ord Account where
  Account a <= Account b = snd a <= snd b

type Bank = [Account]

payed2bank :: PayedFor -> Bank -> Bank
payed2bank (Payed source amount sinks) bank = foldl (flip $ transfer source part) bank sinks
  where part = amount / fromIntegral (length sinks)

payeds2bank :: [PayedFor] -> Bank
payeds2bank = foldl (flip payed2bank) []

-- given a list of accounts, transfer `amount` from `source` to `sink` and create new accounts as neccesary
transfer :: Person -> Rational -> Person -> Bank -> Bank
transfer source amount sink = withdraw source amount . deposit sink amount

-- remove `amount` from `person`s account, create account if neccesary
withdraw :: Person -> Rational -> Bank -> Bank
withdraw person amount = updateBank person (\a -> a - amount)

-- add `amount` to `person`s account, create account if necessary
deposit :: Person -> Rational -> Bank -> Bank
deposit person amount = updateBank person (+ amount)

-- update `person`s account with `updateFunction`, create account if necessary
updateBank :: Person -> (Rational -> Rational) -> Bank -> Bank
updateBank person updateFunction [] = [Account (person, updateFunction 0)]
updateBank person updateFunction (Account (curPerson, balance):rest) =
  if curPerson == person then
    discardEmptyAccounts $ Account (person, updateFunction balance):rest
  else
    discardEmptyAccounts $ Account (curPerson, balance) : updateBank person updateFunction rest

discardEmptyAccounts :: Bank -> Bank
discardEmptyAccounts = filter (\(Account (_, balance)) -> balance /= 0)

clearAll :: Bank -> [Owes]
clearAll bank = debts
  where (debts, _) = clearAll_ ([], bank)
        clearAll_ x@(debts, []) = x
        clearAll_ x             = clearAll_ $ clearOne x

clearOne :: ([Owes], Bank) -> ([Owes], Bank)
clearOne (debts, bankIn) = if null bank then (debts, bank) else (debt:debts, bank_)
  where bank   = bankIn
        Account (richestPerson, richestBalance) = maximum bank
        Account (poorestPerson, poorestBalance) = minimum bank
        amount = min (abs richestBalance) (abs poorestBalance)
        debt   = Owes richestPerson amount poorestPerson
        bank_  = transfer richestPerson amount poorestPerson bank

processAll :: [PayedFor] -> [Owes]
processAll = clearAll . payeds2bank

main = mapM_ print $ sort $ processAll input
