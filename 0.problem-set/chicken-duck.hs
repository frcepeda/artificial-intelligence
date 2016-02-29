import Control.Monad

split (x:xs) = lefts ++ rights
    where rest = split xs
          lefts = map (\(a,b) -> (x:a,b)) rest
          rights = map (\(a,b) -> (a,x:b)) rest
split _ = [([],[])]

pickOne (x:xs) = (x,xs) : map (\(a,b) -> (a,x:b)) (pickOne xs)
pickOne _ = []

main = do
    nums <- map read . words <$> getContents
    forM_ (split nums) $ \(l,r) -> do
        let sr = sum r
        forM_ (pickOne l) $ \(x,l') -> do
            when (sum l' == 2*sr) (print (x,(l', sum l'),(r, sum r)))
