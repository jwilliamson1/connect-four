dbl :: Int -> (Int -> Int) -> Int
dbl x = \f -> f (2 * x)

hlv :: Int -> (Int -> Int) -> Int
hlv x = \f -> f (x `div` 2)

newtype Cont r a = Cont {runCont :: (a -> r) -> r }

instance Functor (Cont r) where
    fmap fn (Cont inC) = Cont $ \out -> inC (out . fn)

instance Applicative (Cont r) where
    pure val = Cont $ \out -> out val
    (Cont fnC) <*> (Cont inC) = Cont $ \out -> fnC(\fn -> inC(out . fn))

instance Monad (Cont r) where
    return = pure
    (Cont inC) >>= fn = Cont $ \out -> inC(\a -> runCont(fn a) out)

callCC :: ((a -> Cont r b)) -> Cont r a) -> Cont r a