data Direction = DLeft
               | DRight
               | DStraight
               deriving (Eq, Show)

data Pnt2D = Pnt2D {
    x :: Double,
    y :: Double
} deriving (Eq, Show)

data Compare = CLess
             | CMore
             | CEqual
             deriving (Eq, Show)

calcDir p1 p2 p3 = case crossProdZ p1 p2 p3 of
                        0           -> DStraight
                        a | a > 0   -> DLeft
                        a | a < 0   -> DRight
    where crossProdZ a b c = (x p2 - x p1) * (y p3 - y p1) - (y p2 - y p1) * (x p3 - x p1)

lowestY [p]    = p
lowestY (p:ps) = compareYthenX p (lowestY ps)
    where compareYthenX a b
              | y a < y b   = a
              | y a > y b   = b
              | otherwise   = if x a < x b
                                 then a
                                 else b

-- p0 is base point that is assumed to be lower then pa and pb
compareAngle p0 pa pb
    | p0 == pa  = CLess
    | p0 == pb  = CMore
    | sameHalf  = if divYX pa < divYX pb
                     then CLess
                     else CMore
    | otherwise = if divYX pa > divYX pb
                     then CLess
                     else CMore
    where divYX p  = (y p - y p0)/(x p - x p0)
          sameHalf = bothRight || bothLeft
              where bothRight = x pa >= 0 && x pb >= 0
                    bothLeft = x pa < 0 && x pb < 0

sortPoints pts = sortWthLwst (lowestY pts) pts
    where sortWthLwst p0 []     = []
          sortWthLwst p0 (p:ps) = smaller ++ [p] ++ bigger
              where smaller = sortWthLwst p0 [x | x<-ps, (compareAngle p0 x p) == CLess]
                    bigger  = sortWthLwst p0 [x | x<-ps, (compareAngle p0 x p) /= CLess]

convexHull (a:b:c) = hullify (residual sorted) (firstTwo sorted)
    where sorted           = sortPoints (a:b:c)
          residual pts     = tail (tail pts)
          firstTwo pts     = [(head pts),(head (tail pts))]
          hullify [] stack = stack
          hullify (a:ps) (s1:s2:ss)
              | calcDir s1 s2 a == DLeft = hullify (ps) (a:s1:s2:ss)
              | otherwise                = hullify (a:ps) (s2:ss)

-- testing
p0 = Pnt2D 0 (-1)
a = Pnt2D 6 0
b = Pnt2D 4 1
c = Pnt2D 3 2
d = Pnt2D 2 4
e = Pnt2D 1 7
f = Pnt2D 0 11
g = Pnt2D 7 (-1)
points = [p0,a,b,c,d,e,f,g]
hull = [f,p0,g]
-- convexHull points == hull

i = Pnt2D 10 10
j = Pnt2D 7 8
k = Pnt2D 0 9
l = Pnt2D (-7) 8
m = Pnt2D (-10) 10
p2 = [p0,i,j,k,l,m]
-- convexHull points == [(-10,10),(0,-1),(10,10)]
