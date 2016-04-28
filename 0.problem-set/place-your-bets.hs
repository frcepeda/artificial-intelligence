import Control.Monad

pickOne (x:xs) = (x,xs) : map (\(a,b) -> (a,x:b)) (pickOne xs)
pickOne _ = []

pickThree xs = do
    (a,aa) <- pickOne xs
    (b,bb) <- pickOne aa
    (c,cc) <- pickOne bb
    guard $ a < b && b < c
    return ([a,b,c], cc)

main = do
    nums <- map read . words <$> getContents
    forM_ (pickThree nums) $ \(t,r) -> do
        let st = sum t
        forM_ (pickThree r) $ \(d,h) -> do
            let sd = sum d; sh = sum h
            when (st == 2*sd) (print ((st,sd,sh),(t,d,h)))
