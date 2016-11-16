-- 1
myLength :: Num a => [t] -> a
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

-- 2
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- 3
myMean [] = error "list to short"
myMean xs = mySum(xs) / fromIntegral (myLength xs)

-- 4
myPal [] = []
myPal (x:xs) = [x] ++ myPal(xs) ++ [x]

-- 5
--isPal [] = True
--isPal [a] = True
--isPal (x:xs)
--    | x == last xs  = isPal (init xs)
--    | otherwise     = False
-- 6
isPal xs
    | null xs               = True
    | length xs == 1        = True
    | head xs == last xs    = isPal (init (tail xs))
    | otherwise             = False

-- 7
intersperse :: a -> [[a]] -> [a]
intersperse a [] = []
intersperse a (x:xs)
    | length xs == 0     = x++intersperse a xs
    | otherwise          = x++[a]++intersperse a xs

-- 8
-- --> se myTree.hs

-- 
