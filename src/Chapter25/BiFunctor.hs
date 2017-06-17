{-# LANGUAGE InstanceSigs #-}

module Chapter25.BiFunctor where

  class BiFunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

  data Deux a b = Deux a b

  instance BiFunctor Deux where
    bimap :: (a -> b) -> (c -> d) -> Deux a c -> Deux b d
    bimap f g (Deux a c) = Deux (f a) (g c)

  data Const a b = Const a

  instance BiFunctor Const where
    bimap :: (a -> b) -> (c -> d) -> Const a c -> Const b d
    bimap f _ (Const a) = Const (f a)

  data Drei a b c = Drei a b c

  instance BiFunctor (Drei a) where
    bimap :: (b -> c) -> (d -> e) -> Drei a b d -> Drei a c e
    bimap f g (Drei a b d) = Drei a (f b) (g d)

  data SuperDrei a b c = SuperDrei a b

  instance BiFunctor (SuperDrei a) where
    bimap :: (b -> c) -> (d -> e) -> SuperDrei a b d -> SuperDrei a c e
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

  data SemiDrei a b c = SemiDrei a

  instance BiFunctor (SemiDrei a) where
    bimap :: (b -> c) -> (d -> e) -> SemiDrei a b d -> SemiDrei a c e
    bimap _ _ (SemiDrei a) = SemiDrei a

  data Quadriceps a b c d = Quadzzz a b c d

  instance BiFunctor (Quadriceps a b) where
    bimap :: (c -> d) -> (e -> f) -> Quadriceps a b c e -> Quadriceps a b d f
    bimap f g (Quadzzz a b c e) = Quadzzz a b (f c) (g e)

  instance BiFunctor Either where
    bimap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
    bimap f _ (Left a)  = Left (f a)
    bimap _ g (Right b) = Right (g b)
