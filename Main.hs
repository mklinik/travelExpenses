-- * A group of people makes a trip.
-- * Someone pays a bill for a subset of the group.
-- * How much does anyone owe to anyone else?

module Main where

import Data.List (nub)

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq)

-- payer, amount, receivers
data PayedFor = PayedFor Person Double [Person]
    deriving Show

data Owes = Owes Person Double Person
    deriving Show

database =
    [ PayedFor Hans 500 [Hans, Klaus, Erna, Elke]
    , PayedFor Klaus 50 [Erna, Elke]
    , PayedFor Elke 10.50 [Hans]
    ]

-- turns a payment into a list of debts
pays2owes :: PayedFor -> [Owes]
pays2owes (PayedFor payer amount receivers) =
    [Owes receiver fraction payer | receiver <- receivers]
        where fraction = amount / (fromIntegral $ length receivers)

allDebts = concatMap pays2owes database

-- normalizeDebts :: [Owes] -> [Owes]
normalizeDebts debts =
    map flup [(filterPair (p1, p2) allDebts, filterPair (p2, p1) allDebts) | (p1, p2) <- pairings participants]

flup :: ([Owes], [Owes]) -> [Owes]
flup ([], xs) = sumDebts xs
flup (xs, []) = sumDebts xs
flup (xs, ys) =
    if amountP1p2 > amountP2p1
        then [Owes p1 (amountP1p2 - amountP2p1) p2]
        else [Owes p2 (amountP2p1 - amountP1p2) p1]
        where (Owes p1 amountP1p2 p2) = head $ sumDebts xs
              (Owes _  amountP2p1 _ ) = head $ sumDebts ys

-- Generates all possible pairings from a list of participants
pairings :: [a] -> [(a, a)]
pairings [] = []
pairings (x:xs) = [(x, y) | y <- xs] ++ (pairings xs)

-- Filters a list of debts by the debts that p1 owes p2
filterPair :: (Person, Person) -> [Owes] -> [Owes]
filterPair (p1, p2) debts = filter (\(Owes x1 _ x2) -> p1 == x1 && p2 == x2) debts

-- Sums all debts of a list of debts.
-- Only works for filtered lists
sumDebts [] = []
sumDebts (x:[]) = [x]
sumDebts ((Owes p1 a p2):(Owes _ b _):xs) = sumDebts ((Owes p1 (a + b) p2):xs)

-- Extracts the list of people who either payed or someone payed for them.
-- Only these persons show up in the final summary
participants :: [Person]
participants = nub $ concat [p:ps | (PayedFor p _ ps) <- database]
