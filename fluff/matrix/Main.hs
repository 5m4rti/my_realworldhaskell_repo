module Main (main) where

import Matrix

m1 = Matrix {values = [1,0,0,1], rows=2, columns=2}
m2 = Matrix {values = [2,0,0,1], rows=2, columns=2}
m3 = Matrix {values = [1..9], rows=3, columns=3}

v1 = Matrix {values = [1,1], rows=2, columns=1}
v3 = Matrix {values = [1,1,1], rows=2, columns=1}

main :: IO ()
main = print (m3 `mul` v3)
