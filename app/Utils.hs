module Utils (FourTuple (..), ftfst, ftsnd, ftthd, ftfth, toList, ftabs, ftabs0, ftabsinf, Addable, MultiplicativeScalar, Subtractible, (*.), (+.), (-.)) where

data FourTuple a = FourTuple a a a a

instance Functor FourTuple where
  fmap f (FourTuple a b c d) = FourTuple (f a) (f b) (f c) (f d)

class Addable a where
  (+.) :: a -> a -> a

class MultiplicativeScalar a where
  (*.) :: a -> FourTuple a -> FourTuple a

class Subtractible a where
  (-.) :: a -> a -> a

instance (Num a) => Addable (FourTuple a) where
  (+.) (FourTuple a b c d)  (FourTuple e f g h) = FourTuple (a + e) (b + f) (c + g) (d + h)

instance MultiplicativeScalar Double where
  (*.) n (FourTuple a b c d) = FourTuple (a * n) (n * b) (n * c) (n * d)

instance (Num a) => Subtractible (FourTuple a) where
  (-.) (FourTuple a b c d) (FourTuple a' b' c' d') = FourTuple (a - a') (b - b') (c - c') (d - d')


ftfst :: FourTuple a -> a
ftfst (FourTuple a _ _ _) = a
ftsnd :: FourTuple a -> a
ftsnd (FourTuple _ a _ _) = a
ftthd :: FourTuple a -> a
ftthd (FourTuple _ _ a _) = a
ftfth :: FourTuple a -> a
ftfth (FourTuple _ _ _ a) = a

toList :: FourTuple a -> [a]
toList (FourTuple a b c d) = [a, b, c, d]

ftabs :: Floating a => FourTuple a -> a
ftabs (FourTuple a b c d) = sqrt (a*a + b*b + c*c + d*d)

ftabs0 :: Floating a => FourTuple a -> a
ftabs0 (FourTuple a b c d) = abs (a + b + c + d)

ftabsinf :: (Ord a, Floating a) => FourTuple a -> a
ftabsinf (FourTuple a b c d) = max (abs a) (max (abs b) (max (abs c) (abs d)))

ftsum :: (Num a) => [FourTuple a] -> FourTuple a
ftsum = foldr (+.) (FourTuple 0 0 0 0)