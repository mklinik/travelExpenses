travelExpenses
==============

Since my first trip with friends, I had the following problem. During the trip,
people randomly pay stuff for other people. For example, Klaus pays for the
cottage for everybody, Hans pays tram tickets, but only for himself, Elke and
Erna.  Erna pays lunch for Elke, and so on. In the end, it's time for everybody
to pay their debts to the other people. Calculating all that by hand, or with a
spreadsheet, is tedious.

Haskell to the resuce.

During the trip, jot down when someone buys something. Then let travelExpenses
determine who owes what to whom.

Usage
-----

1. Edit the list named "input" in Main.hs to include all the bills.
2. Run the program:

        $ runhaskell Main.hs
