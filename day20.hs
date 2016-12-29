import Data.List (sort)
import qualified System.Environment (getArgs)

split :: String -> (Integer, Integer)
split x = (read a, read $ drop 1 b) where (a,b) = break (=='-') x

-- assumes a < c
overlap (a,b) (c,d) | c <= b+1 = True
overlap _ _ = False

merge (a,b) (c,d) = (min a c, max b d)

mergecat x [] = [x]
mergecat x (y:ys) | overlap x y = ((merge x y):ys)
mergecat x (y:ys) = (x:y:ys)

repeatedlyMerge pairs = ms
  where (_, ms) = until (\(a,b) -> length a == length b) -- not efficient
                        (\(a,b) -> (b, foldr mergecat [] b))
                        ([],pairs)

main :: IO ()
main = do
  contents <- getContents
  args <- System.Environment.getArgs
  let ms = repeatedlyMerge $ sort $ map split $ lines contents in
    if elem "--initial" args then
      print $ succ $ snd $ head ms
    else
      print $ sum $ zipWith (\(a,b) (c,d) -> c-(b+1)) ms (tail ms)
