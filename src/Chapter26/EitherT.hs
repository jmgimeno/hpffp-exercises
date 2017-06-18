{-# LANGUAGE InstanceSigs #-}

module EitherT where

  newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

  instance Functor m => Functor (EitherT e m) where
    fmap :: (a -> b) -> EitherT e m a -> EitherT e m b
    fmap f (EitherT me) = EitherT $ (fmap . fmap) f me

  instance Applicative m => Applicative (EitherT e m) where
    pure :: a -> EitherT e m a
    pure = EitherT . pure . pure

    (<*>) :: EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
    (EitherT mf) <*> (EitherT mx) = EitherT $ (<*>) <$> mf <*> mx

  instance Monad m => Monad (EitherT e m) where
    return :: a -> EitherT e m a
    return = pure

    (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    (EitherT meea) >>= f = EitherT $ do
                             eea <- meea
                             case eea of
                               Left e  -> return $ Left e
                               Right a -> runEitherT (f a)
