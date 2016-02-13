import Control.Monad
import Data.List

data List a = E a
            | L [List a]

instance Show a => Show (List a) where
    show (E a) = show a
    show (L l) = show l

infixr 4 :#
data Tree a = a :# [Tree a]

instance Show a => Show (Tree a) where
    show (a :# []) = show a
    show (a :# xs) = show a ++ " " ++ show xs

-- reverse = foldl' (flip (:)) []

reverse' (L l) = L (reverse' <$> reverse l)
reverse' x = x

toTree (L [E r, L ch]) = r :# go ch
    where go ((E x):(L ch):xs) = (x :# go ch) : go xs
          go ((E x):xs) = (x :# []) : go xs
          go [] = []
toTree _ = error "invalid tree"

levelMap :: (a -> b) -> ([c] -> d) -> (b -> d -> c) -> Tree a -> c
levelMap f g h (a :# xs) = h (f a) (g $ map (levelMap f g h) xs)

maximum' [] = 0
maximum' xs = maximum xs

depth = levelMap (const 1) maximum' (+)

degree = levelMap undefined id (\_ xs -> max (length xs) (maximum' xs))

byLevels = takeWhile (not . null) . byLevels'
    where byLevels' = levelMap (:[]) (foldr (zipWith (++)) (repeat [])) (:)

main = do
    let list = L [E 1, E 2, L [E 3, E 4, E 5], E 6, L [E 7, L [L [E 8], E 9]]]
    let ltree = L [E 1, L [E 3, L [E 4, E 5], E 6, E 7, E 8, L [E 9, E 10, L [E 11]]]]

    let exec test args = forM_ args $ \(n, f) -> putStrLn (n ++ ": " ++ f test)

    exec list [("Original", show)
              ,("Reversed", show . reverse')]

    exec ltree [("Tree as list"       , show)
               ,("Tree"               , show . toTree)
               ,("Depth"              , show . depth . toTree)
               ,("Degree"             , show . degree . toTree)
               ,("Flattened by levels", ('\n':) . concat
                                                . intersperse "\n"
                                                . map show
                                                . byLevels
                                                . toTree)]
