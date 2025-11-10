-- Playing around
intMax :: [Integer] -> Integer
intMax [] =  error "empty list"
intMax [x] = x
intMax (x:xs) = max x (intMax xs)

-- Exercise 1.9
mnmInt :: [Integer] -> Integer
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

isIn :: Integer -> [Integer] -> Bool
isIn x [] =  False
isIn x (y:ys) | x == y = True
 | otherwise = isIn x ys 

-- Exercise 1.10
removeFst :: Integer -> [Integer] -> [Integer]
removeFst x [] = []
removeFst x (y:ys) = removeFst2 x (y:ys) []

removeFst2 :: Integer -> [Integer] -> [Integer] -> [Integer]
removeFst2 x [] (z:zs) = (z:zs)
removeFst2 x (y:ys) []
  | x == y = ys
  | otherwise = y : removeFst2 x ys []
removeFst2 x (y:ys) (z:zs) 
  | x == y =  ys ++ (z:zs)
  | otherwise = y: removeFst2 x ys (z:zs)


-- Example 1.11
srtInts :: [Integer] -> [Integer]
srtInts [] = []
srtInts xs = m : (srtInts (removeFst m xs)) where m = mnmInt xs

-- Example 1.12
average :: [Integer] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- Exercise 1.13
{- Write a function count for counting the number of occurrences of a character in a string. -}
count :: Char -> String -> Int
count x "" = 0
count x (y:ys) 
    | x == y = 1 + count x ys
    | otherwise = count x ys

-- Exercise 1.14

repeatC :: Char -> Integer -> String
repeatC x 0 = ""
repeatC x y = [x] ++ repeatC x (y-1)

{- A function for transforming strings into strings is of type String -> String -}
blowup :: String -> String
blowup "" = ""
blowup xs = concat [replicate i c | (i,c) <- zip[1..] xs]

-- Exercise 1.15
{-  Write a function srtString :: [String] -> [String] that sorts a list of strings in alphabetical order. -}

minString :: [String] -> String
minString [] = error "empty list"
minString [x] = x
minString (x: xs) = min x (minString xs)


removeFstS :: String -> [String] -> [String]
removeFstS x [] = []
removeFstS x y = removeFstS2 x y []


removeFstS2 :: String -> [String] -> [String] -> [String]
removeFstS2 x [] (z:zs) = (z:zs)
removeFstS2 x (y:ys) []
  | x == y = ys
  | otherwise = y : removeFstS2 x ys []
removeFstS2 x (y:ys) (z:zs) 
  | x == y =  ys ++ (z:zs)
  | otherwise = y: removeFstS2 x ys (z:zs)

srtString:: [String] -> [String]
srtString [] = []
srtString xs = m : (srtString (removeFstS m xs)) where m = minString xs

-- Example 1.16
{- Suppose we want to check whether a string str1 is a prefix of a string str2. -}

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

-- Exercise 1.17
{- Write a function substring :: String -> String -> Bool that checks whether str1 is a susbtring of str2 -}

