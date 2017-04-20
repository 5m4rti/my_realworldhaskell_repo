squareTR :: [Double] -> [Double]
squareTR xs = squareTR' [] xs
    where squareTR' rs (x:xs) = squareTR' (rs ++ [x*x]) xs
          squareTR' rs []     = rs

