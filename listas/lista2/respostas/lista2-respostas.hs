type Polynomial = [Int]
-- 1
grade :: Polynomial -> Int
grade p = length p - 1

-- 2
-- (a)
normalize :: Polynomial -> Polynomial -> (Polynomial, Polynomial)
normalize p [] = (p, replicate (length p) 0)
normalize [] p = (replicate (length p) 0, p)
normalize p1 p2 = ((fst n) ++ [(last p1)], (snd n) ++ [(last p2)])
    where n = normalize (init p1) (init p2)

-- (b)
(.+.) :: Polynomial -> Polynomial -> Polynomial
(.+.) [] p = p
(.+.) p [] = p
(.+.) p1 p2 = [n + m] ++ ns .+. ms
    where (n:ns, m:ms) = normalize p1 p2

-- 3
derivative :: Polynomial -> Polynomial
derivative [] = []
derivative xs = foldr (\x acc -> [x * (1 + (length acc))] ++ acc) [] (init xs)

-- 4
eval :: Int -> Polynomial -> Int
eval _ [] = 0
eval x as = last as + x * (eval x (init as))
