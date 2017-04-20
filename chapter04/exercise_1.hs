import Data.Char (digitToInt)


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f xs =  [first] ++ (splitWith f (dropWhile f second))
  where (first, second) = break f (dropWhile f xs)

-- second part

asInt_fold :: String -> Int
asInt_fold (c:cs)
    | c == '-'  = (-1) * asInt_fold cs
    | otherwise = foldl add_and_mult 0 (c:cs)
    where add_and_mult acc x = acc*10+ digitToInt x
asInt_fold [] = 0

-- 3
concat :: [[a]] -> [a]
concat xs = foldr step [] xs
    where step x acc = x++acc

-- 4
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pred (x:xs)
    | (pred x) == True  = x:(myTakeWhile pred xs)
    | otherwise         = []

myTkWhFld :: (a -> Bool) -> [a] -> [a]
myTkWhFld f xs = foldr step [] xs
    where step x acc
              | f x       = x:acc
              | otherwise = []


