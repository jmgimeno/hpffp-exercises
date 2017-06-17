{-# LANGUAGE InstanceSigs #-}

module Chapter25.Compose where

  newtype Compose f g a = Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

  instance(Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

  instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fgab) <*> (Compose fga)
      = Compose $ (\gab ga -> gab <*> ga) <$> fgab <*> fga

  instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = foldMap (\ga -> foldMap f ga) fga

  instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: Applicative m => (a -> m b) -> Compose f g a -> m (Compose f g b)
    traverse f (Compose fga) = fmap Compose $ traverse (\ga -> traverse f ga) fga
