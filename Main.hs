-- * A group of people makes a trip.
-- * Repeatedly: someone pays a bill for a subset of the group.
-- * How much does anyone owe to anyone else?

module Main where

import Text.Printf

-- configuration: edit here

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq, Enum, Bounded)

input =
    [ Hans  `Payed` 500 `for` [Klaus]
    , Klaus `Payed` 510 `for` [Hans]
    , Hans  `Payed` 10 `for` [Erna, Elke]
    ]

-- end of configuration: leave the rest as it is

for = ($)

-- Input: payer, amount, receivers
data PayedFor = Payed Person Rational [Person]
    deriving Show

-- Output: the first person owes an amount to the second person
data Owes = Owes Person Rational Person
    deriving Eq

instance Show Owes where
    show (Owes pA amount pB) =
           (show pA) ++ " owes "
        ++ (printf "%.2f" ((fromRational amount)::Double))
        ++ " to " ++ (show pB)

-- turns a payment into a list of debts
pays2owes :: PayedFor -> [Owes]
pays2owes (Payed payer amount receivers) =
    [Owes receiver fraction payer | receiver <- receivers]
        where fraction = amount / (fromIntegral $ length receivers)

allDebts = concatMap pays2owes

-- processOne (x, nonMatchingDebts) d
-- If the people of x and d match, absorb d into x
-- else add it to the list of nonMatchingDebts
processOne :: (Owes, [Owes]) -> Owes -> (Owes, [Owes])
processOne (x@(Owes personA amount personB), nonMatchingDebts) d@(Owes pA am pB)
    | personA == pA && personB == pB = ((Owes personA (amount + am) personB), nonMatchingDebts)
    | personA == pB && personB == pA = ((Owes personA (amount - am) personB), nonMatchingDebts)
    | otherwise                      = (x, d:nonMatchingDebts)

processAll :: [Owes] -> [Owes]
processAll [] = []
processAll (d:ds) = let (e, es) = (foldl processOne (d, []) ds) in
    e : (processAll es)

-- if the amount of a debt is negative, flip the two persons and invert the amount
flipDebt :: Owes -> Owes
flipDebt (Owes pA amount pB) =
    if( amount < 0 )
        then (Owes pB (-amount) pA)
        else (Owes pA amount pB)

main = do
    mapM_ print $ map flipDebt $ filter (\(Owes a _ b) -> a /= b) $ processAll $ allDebts input
