-- my try
lastButOne :: [a] -> a
lastButOne l= if length l <3
             then head l
             else lastButOne (tail l)
