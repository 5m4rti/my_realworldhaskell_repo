data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

data BookReview = BookReview BookInfo CustomerID String

myInfo = Book 987656787654321 "Algebra of Programming"
					["Richard Bird", "Oege de Moor"]
type CustomerID = Int
type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)
