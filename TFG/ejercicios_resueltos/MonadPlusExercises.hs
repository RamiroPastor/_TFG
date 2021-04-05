import Control.Monad

------------------------------------------------------------------------
--              FIRST EXERCISE OF THE MONADPLUS SECTION               --
------------------------------------------------------------------------


-- instance MonadPlus [] where
--   mzero = []
--   mplus = (++)

-- neutral element:
--  mzero `mplus` m = [] ++ m == m
--  m `mplus` mzero = m ++ [] == m

-- associativity
-- (l1 `mplus` l2) `mplus` l3
--   = (l1 ++ l2) ++ l3
--   == l1 ++ (l2 ++ l3)
--   = l1 `mplus` (l2 `mplus` l3)

-- interaction with the monad part

-- mzero >>= f  ==  mzero  ??
-- mzero >>= f
--   = [] >>= f
--   = concatMat f []
--   = []
--   == mzero

-- l >> mzero  ==  mzero  ??
-- l >> mzero
--   = l >> []
--   = l >>= (\_ -> [])
--   = concatMap (\_ -> []) l
--   = []
--   == mzero

-- (l1 `mplus` l2) >>= k
--   ==  ????
-- (l1 >>= k) `mplus` (l2 >>= k)

-- (l1 `mplus` l2) >>= k
--   = (l1 ++ l2) >>= k
--   = concatMap k (l1++l2)
--   == (concatMap k l1) ++ (concatMap k l2)
--   = (l1 >>= k) `mplus` (l2 >>= k)




------------------------------------------------------------------------
--             SECOND EXERCISE OF THE MONADPLUS SECTION               --
------------------------------------------------------------------------


char :: Char -> String -> Maybe (Char, String)
char c s = do
  let (c':s') = s
  if c == c' then Just (c,s') else Nothing


digit :: Int -> String -> Maybe Int
digit i s | i > 9 || i < 0 = Nothing
          | otherwise      = do 
  let (c:_) = s
  if [c] == show i then Just i else Nothing


hexChar :: String -> Maybe Char
hexChar s = (fmap (head . show) (isDigit s)) `mplus` isValidChar 
  where
    funcList    = map digit [0..9]
    isDigit x   = msum (map ($ x) funcList)
    char' c s   = fmap fst (char c s)
    funcList'   = map char' ['a','b','c','d','e','f']
    isValidChar = msum (map ($ s) funcList')