{-# LANGUAGE InstanceSigs #-}

module StateT where

  newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

  instance Functor m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT sma) = StateT $ \s -> fmap (\(a, s) -> (f a, s)) (sma s)


  instance Monad m => Applicative (StateT s m) where
    pure :: a -> StateT s m a
    pure a = StateT $ \s -> return (a, s)

    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    (StateT smf) <*> (StateT sma) = StateT $ \s -> do (f, s') <- smf s
                                                      (a, s'') <- sma s'
                                                      return (f a, s'')

  instance Monad m => Monad (StateT s m) where
    return :: a -> StateT s m a
    return = pure

    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    (StateT sma) >>= f = StateT $ \s -> do (a, s') <- sma s
                                           runStateT (f a) s'
