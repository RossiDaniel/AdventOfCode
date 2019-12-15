

--data :: [Int]
--data = [372304..847060]

int2arr :: Int -> [Int]
int2arr 0 = []
int2arr x = int2arr (x `div` 10) ++ [x `mod` 10]

nextn :: Int -> [Int] -> [Int]
nextn _ [] = []
nextn n (x:xs) | n == x = nextn n xs
               | otherwise = (x:xs)

cond2 :: [Int] -> Bool
cond2 n = ((n!!0) <= (n!!1) && (n!!1) <= (n!!2) && (n!!2) <= (n!!3) && (n!!3) <= (n!!4) && (n!!4) <= (n!!5))

cond1 :: [Int] -> Int
cond1 [] = 0
cond1 (x:xs) | (length (x:xs)) - (length other) == 2 = 1 + cond1 other
             | otherwise                             = cond1 other
             where 
                other = nextn x (x:xs) 

checkn :: [Int] -> Bool
checkn n | (cond2 n) && (cond1 n >= 1)  = True
         | otherwise = False
         
findn :: [Int] -> Int
findn xs = sum [1| x <- xs , checkn (int2arr x) == True]