dbl :: Int -> (Int -> Int) -> Int
dbl x = \f -> f (2 * x)

hlv :: Int -> (Int -> Int) -> Int
hlv x = \f -> f (x `div` 2)

kmap f mw = \cont -> mw (\t -> cont (f t))

kres = kmap (\t -> t * 8) $ hlv 6

kpure part = \cont -> cont part

x = kpure 3

kap fnC inC = \cont -> fnC $ \fn -> inC(cont . fn)

y = kap (dbl 6) (dbl 5)