myNot True = False
myNot False = True

sumList (x:xs) = x + sumList xs
sumList [] = 0

fib 1 = 1
fib 2 = 1
fib x = fib(x-1)+fib(x-2)
