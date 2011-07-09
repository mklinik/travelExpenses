-- * A group of people makes a trip.
-- * Repeatedly: someone pays a bill for a subset of the group.
-- * How much does anyone owe to anyone else?

module Main where

-- configuration: edit here

data Person = Hans | Klaus | Erna | Elke
    deriving (Show, Eq)

database =
    [ PayedFor Hans 500 [Klaus]
    , PayedFor Klaus 510 [Hans]
    , PayedFor Hans 10 [Erna, Elke]
    ]

-- end of configuration: leave the rest as it is

-- payer, amount, receivers
data PayedFor = PayedFor Person Double [Person]
    deriving Show

-- the first person owes an amount to the second person
data Owes = Owes Person Double Person
    deriving Show

-- turns a payment into a list of debts
pays2owes :: PayedFor -> [Owes]
pays2owes (PayedFor payer amount receivers) =
    [Owes receiver fraction payer | receiver <- receivers]
        where fraction = amount / (fromIntegral $ length receivers)

allDebts = concatMap pays2owes database

-- Takes a debt x and a list of debts ds.
-- For each debt in ds: check if the two persons match the two persons in x,
-- and update x accordingly.
-- Returns the updated debt with all debts of ds absorbed into x, and the rest
-- list which contains no more debts between the two persons of x.
processOne :: Owes -> [Owes] -> (Owes, [Owes])
processOne x [] = (x, [])
processOne (Owes personA amount personB) ((Owes pA am pB):ds) =
         if( personA == pA && personB == pB ) then processOne (Owes personA (amount + am) personB) ds
    else if( personA == pB && personB == pA ) then processOne (Owes personA (amount - am) personB) ds
    else (updatedDebt, (Owes pA am pB):rest)
        where (updatedDebt, rest) = processOne (Owes personA amount personB) ds

processAll :: [Owes] -> [Owes]
processAll [] = []
processAll (d:ds) = updatedDebt : (processAll rest)
    where (updatedDebt, rest) = processOne d ds

-- if the amount of a debt is negative, flip the two persons and invert the amount
flipDebt :: Owes -> Owes
flipDebt (Owes pA amount pB) =
    if( amount < 0 )
        then (Owes pB (-amount) pA)
        else (Owes pA amount pB)

main = do
    putStrLn $ show $ map flipDebt $ processAll allDebts
