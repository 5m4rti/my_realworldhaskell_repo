module Matrix
  ( Matrix(..)
  , isValid
  , add
  , row
  , col
  , mul
  ) where

data Matrix = Matrix
  { values :: [Double]
  , rows :: Int
  , columns :: Int
  } deriving (Show)

isValid :: Matrix -> Bool
isValid m = length (values m) == rows m * columns m

add :: Matrix -> Matrix -> Matrix
add m n
  | rows m == rows n && columns m == columns n =
    let v = zipWith (+) (values m) (values n)
    in Matrix {values = v, rows = rows m, columns = columns m}
  | otherwise = error "matrice shapes not compatible"

row :: Matrix -> Int -> [Double]
row m r = take c (drop (c * (r - 1)) v)
  where
    c = columns m
    v = values m

col :: Matrix -> Int -> [Double]
col m r = reverse (colRec vals 1 [])
  where
    vals = values m
    colRec [] _ result = result
    colRec (v:vs) count result
      | count `mod` columns m == r `mod` columns m =
        colRec vs (count + 1) (v : result)
      | otherwise = colRec vs (count + 1) result

mul :: Matrix -> Matrix -> Matrix
mul m n =
  let newRows = rows m
      newCols = columns n
      valueIndices =
        concatMap (\r -> [(r, c) | c <- [1 .. newCols]]) [1 .. newRows]
      matrixValue (r, c) = sum $ zipWith (*) (row m r) (col n c)
  in Matrix
     {values = map matrixValue valueIndices, rows = newRows, columns = newCols}    
