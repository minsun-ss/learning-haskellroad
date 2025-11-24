-- Exercise 3.2
{- Apply both immplication rules to prove P -> R from the given P -> Q, P -> (Q -> R)

Given: P -> Q, P -> (Q -> R)
To be proved: P -> R
Proof:
    Suppose P
    From P -> Q and P, conclude Q.
    From p -> (Q -> R), conclude Q -> R.
    From Q -> R and Q, conclude R.
-}

-- Exercise 3.4
{- Assume n, m E N. Show (m is odd && n is odd) -> m + n is even.

Given: n is odd, m is odd.
Thus, n and m is odd. 
p, q E N exist such that n = 2p + 1, m = 2q + 1.
m + n = 2p + 2q + 2 = 2(p + q + 1) is even.
-}

-- Exercise 3.5 
{- Show
    1. From P <-> Q it follows that (P -> R) <-> (Q -> R).
Given: P <-> Q.
Proof:
    Suppose P -> R.
    From P <-> Q, P == Q.
    From P -> R and P == Q, Q -> R.

    Suppose Q -> R
    From P <-> Q, P == Q.
    From Q -> R and P == Q, P -> R.

2. From P <-> Q it follows that (R -> P) <-> (R -> Q).

Given: P <-> Q.
Proof:
    Suppose R -> P.
    From P <-> Q, P == Q.
    From R -> P and P == Q, R -> Q.

    Suppose R -> Q.
    From P <-> Q, P == Q.
    From R -> Q and P == Q, R -> P.
-}

-- Exercise 3.7
{- Given P -> Q. To show, ~Q -> ~P.
Proof:
    Suppose not (~Q -> ~P).
    From not(~Q -> ~P), ~Q and P.
    From ~Q and P, ~Q, P.
    From P -> Q and P, Q.
    Q, ~Q is a contradiction. 

Given P <-> Q. To show, ~P <-> ~Q.
Proof:
    Suppose ~ (~P <-> Q).
    From ~(~P <-> Q), P XOR Q.
    From P XOR Q, P != Q.
    From P <-> Q, P == Q.
    P == Q and P != Q is a contradiction.
-}

-- Exercise 3.9
{-
Show from (P -> Q) -> P, it follows P.
Proof:
    Suppose ~P.
    From (P -> Q) -> P and ~P, ~(P -> Q).
    From ~(P -> Q), P && ~Q.
    From P && ~Q, P, ~Q.
    From P and ~P, contradiction.
-}

-- Exercise 3.11
{-
Show from A-> B || C and B -> ~A, A-> C.
Proof:
   A -> B || C = A -> B or A -> C.
   Suppose A -> B.
   From A -> B, B -> ~A, then A -> ~A, which means A -> C.

   Suppose A -> C. Then A -> C holds by assumption.

From given A || B -> C || D, C -> A and B -> ~A, derive B -> D.
    Suppose B.
    With B -> ~A and B, then ~A.
    C -> A and ~A, then ~C.
    A || B, ~A, B, then B.
    C || D and ~C, then D.
    Therefore B -> D.
-}

-- Exercise 3.15
{- Show that for any n E N, divison n^2 by 4 gives remainder 0 or 1. -}
{- 
For all n E N, n ^ 2 = n + n... n times. 

For all even numbers, there exists p where n = 2p. n^2 = 2p * 2p = 4p^2, which is divisable by 4 with remainder 0.
For all odd numbers, there exists q where n = 2q+1. (2q+1)^2 = 4q^2 + 4q + 1 = 4(q^2+q) + 1, which is divisible by 4 with remainder 1.
    -}

-- Exercise 3.17
{- ?? -}

{- skipped a bunch -}

-- Exercise 3.34

{- 
prime :: Integer -> Bool
prime n 
  | n < 1 = error "not a positive integer"
  | n == 1 = False
  | otherwise = ldp n == n where
    ldp = ldpf primes
    ldpf (p:ps) m 
      | rem m p == 0 = p
      | p^2 > m = m
      | otherwise = ldpf ps m
    primes = 2 : filter prime [3..]
-}

sieve :: [Integer] -> [Integer]
sieve (0:xs) = sieve xs
sieve (n : xs) = n : sieve (mark xs 1 n)
    where
    mark :: [Integer] -> Integer -> Integer -> [Integer]
    mark (y:ys) k m 
      | k == m = 0: (mark ys 1 m)
      | otherwise = y : (mark ys (k+1) m)

primes :: [Integer]
primes = sieve [2..]

-- Exercise 3.38
fasterprimes :: [Integer] -> [Integer]
fasterprimes (0:xs) = fasterprimes xs
fasterprimes (n:xs) = n : fasterprimes (mark xs 1 n)
    where
    mark :: [Integer] -> Integer -> Integer -> [Integer]
    mark (y:ys) k m
      | k == m = 0 : (mark ys 1 m)
      | otherwise = y : (mark ys (k+1) m)


oddsFrom3 :: [Integer]
oddsFrom3 = 3 : map (+2) oddsFrom3

primes2 :: [Integer]
primes2 = fasterprimes oddsFrom3

refutePrimes :: Int -> Bool
refutePrimes n = isPrime (product (take (fromIntegral n) primes) + 1)
    where
    isPrime n 
      | n < 2 = False
      | n == 2 = True
      | even n = False
      | otherwise = all (\p -> n `mod` p /= 0) [3, 5..floor (sqrt (fromIntegral n))]

-- Exercise 3.42
{-
There exists a q where p = 2q+1, 2q+3, 2q+5 in which one of these three values are divisible by 3, and therefore not prime with the exception of the original prime triple, 3, 5, 7. 2q+1 mod 3 can only equal 0, 1 or 2; in the case of 0, it's divisible by three. In the case of 1, adding 2 more will make it divisible by three, and in the case of 2, adding 4 will make it divisible by three. 

-}

-- Exercise 3.43
{- 
 
All primes are in the form of x + x... x times. ....??? 
 -}
