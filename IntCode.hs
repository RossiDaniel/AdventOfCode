module IntCode where

int2arr :: Int -> [Int]
int2arr 0 = []
int2arr x = int2arr (x `div` 10) ++ [x `mod` 10]

zeros :: Int -> [Int]
zeros 0 = []
zeros x = 0 : zeros (x-1)

int2istr :: Int -> [Int]
int2istr xs = reverse ((zeros (5 - (length exp))) ++ exp)
            where
                exp = int2arr xs

moderead :: [Int] -> Int -> Int -> Int -> Int
moderead xs i m b | m == 0    = xs!!(xs!!i)
                  | m == 1    = xs!!i
                  | m == 2    = xs!!(b+(xs!!i))
                  | otherwise = 0

modewrite :: [Int] -> Int -> Int -> Int -> Int
modewrite xs i m b | m == 0    = xs!!i
                   | m == 2    = b+(xs!!i)
                   | otherwise = 0

somma :: [Int] -> Int -> [Int] -> Int -> [Int] 
somma xs i istr b= substitute xs (modewrite xs (i+3) (istr!!4) b) ((moderead xs (i+1) (istr!!2) b) + (moderead xs (i+2) (istr!!3) b))

prodotto :: [Int] -> Int -> [Int] -> Int -> [Int]
prodotto xs i istr b = substitute xs (modewrite xs (i+3) (istr!!4) b) ((moderead xs (i+1) (istr!!2) b) * (moderead xs (i+2) (istr!!3) b))

inputistr :: [Int] -> Int -> [Int] -> Int -> [Int] -> [Int]
inputistr xs i istr b inp= substitute xs (modewrite xs (i+1) (istr!!2) b) (head inp)

outputistr :: [Int] -> Int -> [Int] -> Int ->  Int
outputistr xs i istr b= moderead xs (i+1) (istr!!2) b

notequal0 :: [Int] -> Int -> [Int] -> Int -> Int
notequal0 xs i istr b | (moderead xs (i+1) (istr!!2) b) /= 0 = moderead xs (i+2) (istr!!3) b
                      | otherwise = i+3

equal0 :: [Int] -> Int -> [Int] -> Int -> Int
equal0 xs i istr b | (moderead xs (i+1) (istr!!2) b) == 0 = moderead xs (i+2) (istr!!3) b
                   | otherwise = i+3

less :: [Int] -> Int -> [Int] -> Int -> [Int] 
less xs i istr b | (moderead xs (i+1) (istr!!2) b) < (moderead xs (i+2) (istr!!3) b) = substitute xs (modewrite xs (i+3) (istr!!4) b) 1
                 | otherwise = substitute xs (modewrite xs (i+3) (istr!!4) b) 0

equal :: [Int] -> Int -> [Int] -> Int -> [Int]
equal xs i istr b | (moderead xs (i+1) (istr!!2) b) == (moderead xs (i+2) (istr!!3) b) = substitute xs (modewrite xs (i+3) (istr!!4) b) 1
                  | otherwise = substitute xs (modewrite xs (i+3) (istr!!4) b) 0

upbase :: [Int] -> Int -> [Int] -> Int -> Int
upbase xs i istr b = b + (moderead xs (i+1) (istr!!2) b)

substitute :: [Int] -> Int -> Int -> [Int]
substitute xs i v = subgood xs v 0 i

subgood :: [Int] -> Int -> Int -> Int -> [Int]
subgood [] v _ _ = [v]
subgood (x:xs) v c a | c == a    = v:xs
                     | otherwise = x:(subgood xs v (c+1) a)

op :: [Int] -> [Int] -> Int -> Int -> (([Int],[Int],Int),[Int])
op inp xs i b| istr!!0 == 1 && istr!!1 == 0 = (op inp (somma xs i istr b) (i+4) b)
             | istr!!0 == 2 && istr!!1 == 0 = (op inp (prodotto xs i istr b) (i+4) b)
             | istr!!0 == 3 && istr!!1 == 0 = (op (drop 1 inp) (inputistr xs i istr b inp) (i+2) b)
             | istr!!0 == 4 && istr!!1 == 0 = ((inp,xs,(i+2)),[(outputistr xs i istr b)])
             | istr!!0 == 5 && istr!!1 == 0 = (op inp xs (notequal0 xs i istr b) b)
             | istr!!0 == 6 && istr!!1 == 0 = (op inp xs (equal0 xs i istr b) b)
             | istr!!0 == 7 && istr!!1 == 0 = (op inp (less xs i istr b) (i+4) b)
             | istr!!0 == 8 && istr!!1 == 0 = (op inp (equal xs i istr b) (i+4) b)
             | istr!!0 == 9 && istr!!1 == 0 = (op inp xs (i+2) (upbase xs i istr b))
             | otherwise                    = ((inp,[],0),[0])
             where
                istr = int2istr (xs!!i)
