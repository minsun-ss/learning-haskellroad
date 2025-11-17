-- Exercise 2.2
{- Make up the truth table for the exclusive version of or. -}
{-
T T F
T F T
F T T
F F F
    -}

-- Exercise 2.9
{- Show (P XOR Q) XOR Q == P -}
{- 

P XOR Q
T F   T
T T   F
F T   T
F F   F

XOR XOR Q
F   T   T
T   T   F
T   F   T
F   F   F

2nd XOR == P.
    -}

-- Exercise 2.11
{- Use the method by hand to prove the other parts of Theorem 2.10 -}
{- law of double negation

P ~P ~~P
T  F   T
F  T   F
-}
{- laws of idempotence
P OR P
T T  T
F F  F

P AND P
T T   T
F F   F
-}

{- laws of contraposition
~P -> ~Q == Q -> P

P Q ~P -> ~Q   Q -> P   
T T F  T  F    T  T  T
T F F  T  T    F  T  T
F T T  F  F    T  F  F
F F T  T  T    F  T  F -}

{- laws of commutativity 
P && Q == Q && P
T T  T    T  T T
T F  F    F  F T
F F  T    T  F F
F F  F    F  F F
    -}

{- de morgan laws 
~(P && Q) == ~P || ~Q
F T T  T      F  F  F 
T T F  F      F  T  T
T F F  T      T  T  F
T F F  F      T  T  T
-}

{- laws of associativity 
P && (Q && R) == (P && Q) && R
T  T  T T  T      T T  T  T  T
T  F  F F  T      T F  F  F  T
T  F  T F  F      T T  T  F  F
T  F  F F  F      T F  F  F  F
F  F  F F  F      F F  T  F  F
F  F  T F  F      F F  F  F  F
F  F  T T  T      F F  T  F  T
F  F  F F  T      F F  F  F  T
-}

{- distribution laws
P && (Q OR R) == (P && Q) || (P && R)
T  T  T T  T      T T  T  T   T T  T
T  F  F F  T      T F  F  F   T F  T
T  T  T T  F      T T  T  T   T F  F
T  F  F F  F      T F  F  F   T F  F
F  F  T T  T      F F  T  F   F F  T     
F  F  F T  T      F F  F  F   F F  T 
F  F  T T  F      F F  T  F   F F  F
F  F  F F  F      F F  F  F   F F  F
-}

-- Exercise 2.14
{- Implement checks for the principles from Theorem 2.12 -}
infix 1 ==>
(==>) :: Bool -> Bool -> Bool
x ==> y = (not x) || y

infix 1 <=>
(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

infixr 2 <+>
(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 = (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [(bf1 p q) <=> (bf2 p q) |  p <- [True,False], 
                                                    q <- [True, False]]

logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [(bf1 p q r) <=> (bf2 p q r) |  p <- [True, False],
                                                        q <- [True, False],
                                                        r <- [True, False]]

test2121a   = logEquiv1 (\p -> True) (\p -> (not False))
test2121b   = logEquiv1 (\p -> False) (\p -> (not True))
test2122    = logEquiv1 (\p -> p ==> False) (\p -> (not p))
-- dominance laws
test2123a   = logEquiv1 (\p -> (p || True)) (\p -> True)
test2123b   = logEquiv1 (\p -> (p && False)) (\p -> False)
-- identity laws
test2124a   = logEquiv1 (\p -> p || False) (\p -> p)
test2124b   = logEquiv1 (\p -> p && True) (\p -> p)
-- law of excluded middle
test2125    = logEquiv1 (\p -> p || (not p)) (\p -> True)
-- contradiction
test2126    = logEquiv1 (\p -> p && (not p)) (\p -> False)

-- Exercise 2.15
{- A proprositional contradiction is a formula that yields false for every combination of true values for its proposition letters. Write Haskell definitions of contradiction tests for propositional functions with one, two and three variables -}

test215a    = logEquiv1 (\p -> p && False) (\p -> False)
test215b    = logEquiv2 (\p q -> (p && False) && q) (\p q -> False)    
test215c    = logEquiv3 (\p q r -> (p && False) && q && r) (\p q r -> False)

-- Exercise 2.16
{- Produce useful denials for every sentence of Exercise 2.31 -}
{- 
1. The equation x^2 + 1 has a solution. The equation x^2 +1 has no solution.
2. A largest natural number does not exist. There is at least one natural number larger than the known largest natural number.
3. The number 13 is a prime (use d|n for d divides n). The number 13 is not a prime.
4. The number n is a prime. The number n is not a prime.
5. There are infinitely many primes. There no prime bigger than the largest known prime.
    -}

-- Exercise 2.17
{- Produce a denial for the statement that x < y < z (when x, y z E R).

(x < y) < z

x !< y

-}

-- Exercise 2.18
{- Show -}

-- iff p q == iff ~p ~q
test218a = logEquiv2 (\p q -> p <=> q) (\p q -> (not p) <=> (not q))
-- iff ~p q == iff p ~q
test218b = logEquiv2 (\p q -> (not p) <=> q) (\p q -> p <=> (not q))

-- Exercise 2.19
{- Show that p == q iff p <=> q is logically valid. -}
test219     = logEquiv2 (\p q -> (p == q) <=> (p <=> q)) (\p q -> True)

-- Exercise 2.20
{- Determine either using true tables that the following are equivalent, next check answer by computer -}
{- 1. ~p -> q and p -> ~q NOT EQUIVALENT -}
test2201 =  logEquiv2 (\p q -> (not p) ==> q) (\p q -> p ==> (not q))
{- 2. ~p -> q and q -> ~p NOT EQUIVALENT -}
test2202 = logEquiv2 (\p q -> (not p) ==> q) (\p q -> q ==> (not p))
{- 3. ~p -> q and ~q and p EQUIVALENT, law of contraposition -}
test2203 = logEquiv2 (\p q -> (not p) ==> q) (\p q -> (not q) ==> p)
{- 4. p -> (q -> r) and q -> (p -> r) EQUIVALENT ??? -}
{- p -> (q -> r)
   p -> ~q || r
   q -> ~p || r
   q -> (p -> r) -}
test2204 =  logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> q ==> (p ==> r))
{- 5. p -> (q -> r) and (p -> q) -> r NOT EQUIVALENT -}
test2205 =  logEquiv3 (\p q r -> p ==> (q ==> r)) (\p q r -> (p ==> q) ==> r)
{- 6. (p -> q) -> p and p EQUIVALENT -}
test2206 = logEquiv2 (\p q -> (p ==> q) ==> p) (\p q -> p)
{- 7. p || q -> r and (p -> r) && (q -> r) EQUIVALENT -}
test2207 = logEquiv3 (\p q r -> (p || q ==> r)) (\p q r -> (p ==> r) && (q ==> r))

-- Exercise 2.21
{- 1. Construct a formula: ~P -> ~Q
   2. How many truth tables are there for 2 letter formulas altogether? 16
   3. Can you find formulas for all of them?
   TTTT = (P || ~P) || Q
   TTTF = (P || ~P) && Q
   TTFT = (P || ~P) && ~Q
   TTFF = (Q || ~Q) && P
   TFTT = P -> Q
   TFTF = (Q || ~Q) || P
   TFFT = ~(P XOR Q)
   TFFF = P && Q
   FTTT = ~ (TFFF)
   FTTF = ~ (TFFT)
   FTFT = ~ (TFTF)
   FTFF = ~ (TFTT)
   FFTT = ~ (TTFF)
   FFTF = ~ (TTFT)
   FFFT = ~ (TTTF)
   FFFF = ~ (TTTT)
   4. Is there a general method to finding these formulas? Maybe but can't think right now
   5. What about 3 letter formulas - 2 ^ 8
   -}

-- Exercise 2.22 
{- Can you think of an argument showing that statement 2.1 is true? - The distance halfway between two rational numbers is yet another rational number. -}

-- Exercise 2.23
{- Give structure trees ... but I don't want to :< -}

-- Exercise 2.31
{- Translate into formulas ... -}

-- Exercise 2.32
{- Translate into formulas:
1. Everyone loved Diana.
    P = people
    For all x in P(x ==> L(x,d))
2. Diana loved everyone.
    For all x in P(d ==> L(d,x))
3. Man is mortal.
    For all x in P(x ==> M(x), M(x) ==> M'(x))
4. Some birds to not fly.
    For some birds in B(x ==> B(x), B(x) ==> ~F(x))
    -}

{- ... skipped a lot of arguing here -}

-- Exercise 2.51
{- Define a function unique that gives True for unique p xs just in case there is exactly one object among xs that satisfies p. -}
unique :: (a -> Bool) -> [a] -> Bool
unique a xs = length (counter a xs) == 1
  where 
    counter a []  = []
    counter a (x:xs) | a x = x: counter a xs
                    | otherwise = counter a xs


unique2 :: (a -> Bool) -> [a] -> Bool
unique2 a xs | sum(map (\x -> if a x then 1 else 0) xs) == 1 = True
             | otherwise = False

-- Exercise 2.52
{-- Define a function parity :: [Bool] -> Bool that gives True for partiy xs just in case an even number of xss equal True --}

parity :: [Bool] -> Bool
parity xs | rem (sum (map (\x -> if x then 1 else 0) xs)) 2 == 0 = True
            | otherwise = False
 
-- Exercise 2.53
{- Define a function evenNR :: (a -> Bool) -> [a] -> Bool that gives True for evenNR p xs just in case an even number of the xs have property p -}
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR a xs = parity (map (\x -> a x) xs)

