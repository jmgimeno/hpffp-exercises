{-# LANGUAGE InstanceSigs #-}

module IdentityT where

  newtype IdentityT m a = IdentityT { runIdentityT :: m a }

  instance Functor m => Functor (IdentityT m) where
    fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
    fmap f (IdentityT ma) = IdentityT $ fmap f ma

  instance Applicative m => Applicative (IdentityT m) where
    pure :: a -> IdentityT m a
    pure = IdentityT . pure

    (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    (IdentityT mf) <*> (IdentityT ma) = IdentityT $ mf <*> ma

  instance Monad m => Monad (IdentityT m) where
    return :: a -> IdentityT m a
    return = pure

    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (IdentityT ma) >>= f = IdentityT $ do a <- ma
                                          runIdentityT (f a)
