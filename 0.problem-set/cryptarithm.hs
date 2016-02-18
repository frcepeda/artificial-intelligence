import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M

(<<) = flip (>>)

type Cryptarithm a = StateT (M.Map Char Int) [] a

data Op = Plus | Times
toFunc Plus = sum
toFunc Times = product

data Expr = Expr Op [String] String
usedVars (Expr _ xs x) = nub $ x ++ concat xs

toNum :: String -> Cryptarithm Int
toNum s = do
    assignments <- get
    return $ foldl' (\n c -> 10*n + fromJust (M.lookup c assignments)) 0 s

works (Expr op args resS) = do
   nums <- mapM toNum args
   res <- toNum resS
   return $ res == (toFunc op) nums

allPos expr = do
   let vars = usedVars expr
   forM vars $ \c -> do
      used <- M.elems <$> get
      o <- lift $ [0..9] \\ used
      modify $ M.insert c o

solve expr = guard =<< works expr << allPos expr

main = forM_ [1..] $ \i -> do
    [args,ans,op] <- replicateM 3 getLine

    let theOp = if op == "+" then Plus else Times
    let expr = Expr theOp (words args) ans
    let assocs = execStateT (solve expr) M.empty

    putStrLn $ "Case #" ++ show i ++ ":"

    if null assocs
    then putStrLn "No Solution."
    else print $ head assocs
