-- 1
double :: Int -> Int
double = (*2)

-- 2
double2 :: Int -> Int
double2 = double . double

-- 3
sel :: Bool -> Int -> Int -> Int
sel x a b
    | x         = b
    | otherwise = a

-- 4
-- (a)
max2 :: Int -> Int -> Int
max2 x y = sel (y > x) x y

-- (b)
max2' :: Int -> Int -> Int
max2' x y
    | x > y     = x
    | otherwise = y

-- 5
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 x (max2 y z)

-- 6
eq2 :: Int -> Int -> Bool
eq2 = (==)

-- 7
diferent3 :: Int -> Int -> Int -> Bool
diferent3 x y z = x /= y && x /= z && y /= z

-- 8
-- (a)
gcd' :: (Integral a) => a -> a -> a
gcd' x 0 = x
gcd' x y = gcd y (x `mod` y)

-- (b)
lcm' :: (Integral a) => a -> a -> a
lcm' x y = x * y `div` (gcd' x y)

-- 9
m :: (Integral a) => a -> a
m x
    | x > 100   = x - 10
    | otherwise = m (m (x + 11))
-- retorna 91 para valores menores que 101

-- 10
-- (a)
multf :: (Int, Int) -> (Int, Int) -> (Int, Int)
multf (an, ad) (bn, bd) = (an * bn, ad * bd)

-- (b)
somaf :: (Int, Int) -> (Int, Int) -> (Int, Int)
somaf (an, ad) (bn, bd) = (an * d `div` ad + bn * d `div` bd, d)
    where d = lcm' ad bd

-- (c)
subf :: (Int, Int) -> (Int, Int) -> (Int, Int)
subf (an, ad) (bn, bd) = (an * d `div` ad - bn * d `div` bd, d)
    where d = lcm' ad bd

-- (d)
divf :: (Int, Int) -> (Int, Int) -> (Int, Int)
divf (an, ad) (bn, bd) = (an * bd, ad * bn)

-- (e)
toReal :: (Int, Int) -> Float
toReal (n, d) = (fromIntegral n) / (fromIntegral d)

-- 11
intercala :: [Int] -> [Int] -> [Int]
intercala [] _ = []
intercala (x:xs) [] = [x]
intercala (x:xs) (y:ys) = [x, y] ++ intercala xs ys

-- 12
quads :: Int -> Int -> [Int]
quads x y
    | x > y     = []
    | otherwise = [x * x] ++ quads (x + 1) y

-- 13
sumQuads :: Int -> Int -> Int
sumQuads x y = sum (quads x y)

-- 14
divisors :: Int -> [Int]
divisors x = [d | d <- [1..x], x `mod` d == 0]

-- 15
perfect :: Int -> Bool
perfect x = sum (divisors x) == x

-- 16
prime :: Int -> Bool
prime x = length (divisors x) == 2

-- 17
primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

-- 18
media :: [Int] -> Float
media l = (fromIntegral (sum l)) / (fromIntegral (length l))

-- 19
palindrome :: (Eq a) => [a] -> Bool
palindrome l = l == reverse l

-- 20
toPalindrome :: (Eq a) => [a] -> [a]
toPalindrome l
    | palindrome l = l
    | otherwise    = l ++ reverse l
