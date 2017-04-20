
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail [] = error "empty list"
myTail (x:xs) = xs

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myInit :: [a] -> [a]
myInit [] = error "empty list"
myInit [x] = []
myInit (x:xs) = x:myInit(xs)

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x:(myAppend xs ys)

listAnd :: [Bool] -> Bool
listAnd []     = True
listAnd (x:xs) = x && (listAnd xs)

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred []     = True
myAll pred (x:xs) = (pred x) && (myAll pred xs)

myTake :: Int -> [a] -> [a]
myTake 0 xs     = []
myTake n []     = []
myTake n (x:xs) = x:(myTake (n-1) xs)

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x:xs) = myDrop (n-1) xs

mySplitAt :: Int -> [a] -> ([a], [a])
mySplitAt n xs = (take n xs, drop n xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile pred (x:xs)
    | (pred x) == True  = x:(myTakeWhile pred xs)
    | otherwise         = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile pred (x:xs)
    | (pred x) == True  = myDropWhile pred xs
    | otherwise         = x:xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter pred (x:xs)
    | pred x    = x:(myFilter pred xs)
    | otherwise = myFilter pred xs
