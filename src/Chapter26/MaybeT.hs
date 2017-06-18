{-# LANGUAGE InstanceSigs #-}

module MaybeT where

  newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

  instance Functor m => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f (MaybeT mma) = MaybeT $ (fmap . fmap) f mma

  instance Applicative m => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure = MaybeT . pure . pure

    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (MaybeT mmf) <*> (MaybeT mma) = MaybeT $ (<*>) <$> mmf <*> mma

  instance Monad m => Monad (MaybeT m) where
    return :: a -> MaybeT m a
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (MaybeT mma) >>= f = MaybeT $ do
                           ma <- mma
                           case ma of
                             Nothing -> return Nothing
                             Just a  -> runMaybeT (f a)
