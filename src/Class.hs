module Class (HasProd (..), HasInjection (..), HasUnit(..), HasNum(..)) where

-- Classes

class HasUnit a where
  tt :: a

class HasNum a where
  num :: Int -> a

class HasProd a where
  prod :: a -> a -> a

class HasInjection a where
  inl :: a -> a
  inr :: a -> a
