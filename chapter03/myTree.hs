-- Nullable type
--data Maybe a = Just a
--             | Nothing

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

-- 8
height (Node _ Nothing Nothing) = 1
height (Node _ (Nothing) (Just b)) = 1 + (height b)
height (Node _ (Just a) (Nothing)) = 1 + (height a)
height (Node _ (Just a) (Just b)) = 1 + (max (height a) (height b))
