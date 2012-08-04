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

-- turns a payment into a list of debts
pays2owes :: PayedFor -> [Owes]
pays2owes (Payed payer amount receivers) =
    [Owes receiver fraction payer | receiver <- receivers]
        where fraction = amount / (fromIntegral $ length receivers)

allDebts = concatMap pays2owes

processAll = processAll' . map normalize

-- use the first Element of the list to make a simplification
-- if possible simplify again otherwise simplify the remaining list
processAll' :: [Owes] -> [Owes]
processAll' [] = []
processAll' (x:xs) =
    case processOne (x,xs) of
      -- x was used in a simplification
      (True,ys) -> processAll ys
       -- x cannot be used in a simplification
      (False,ys) ->  x : processAll ys

-- argument consits of fixed element which should be used in a simplification
-- and a list of the remaining elements
-- returns (True, simplified list) if simplification is possible and
-- (False, original list) otherwise
processOne :: (Owes, [Owes]) -> (Bool,[Owes])
processOne (x@(Owes pA amnt pB),xs)
    -- in this case we can just discard x
    | pA == pB || amnt == 0 = (True,xs)
    | xs == [] = (False,[])
    | otherwise = processHelper xs
              -- recurse of the remaining elements to find elements where simpfication is possible
        where processHelper [] = (False,[])
              processHelper (y@(Owes pC amnt' pD):ys)
                    -- both persons equal - when just add the dept
                  | pA == pC && pB == pD = (True,(Owes pA (amnt + amnt') pB):ys)
                    -- if person A owes x to B and B owes y to C then
                    -- person A owes (min x y) to C and either A owes |x-y| To B or B owes |x-y| to C
                  | pB == pC = (True,combine x y ++ ys)
                  | pD == pA = (True,combine y x ++ ys)
                  | otherwise = let (b,l) = processHelper ys in
                                (b,y:l)
                        -- assumes pB == pC
                  where combine (Owes pA amnt pB) (Owes pC amnt' pD) =
                            if amnt == amnt' then
                                [Owes pA amnt pD]
                            else if amnt < amnt' then
                                     [(Owes pA (min amnt amnt') pD),(Owes pC (amnt'-amnt) pD)]
                                 else
                                     [(Owes pA (min amnt amnt') pD),(Owes pA (amnt-amnt') pB)]

normalize :: Owes -> Owes
normalize x@(Owes personA amount personB)
  | amount < 0 = (Owes personB (-amount) personA)
  | otherwise  = x

main = do
    mapM_ print $ sort $ processAll $ allDebts input
