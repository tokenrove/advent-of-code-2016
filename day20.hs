import Data.List (sort)
import qualified System.Environment (getArgs)

type Range = (Integer, Integer)

split :: String -> Range
split x = (read a, read $ drop 1 b) where (a,b) = break (=='-') x

-- assumes a < c
overlap :: Range -> Range -> Bool
overlap (_,b) (c,_) | c <= b+1 = True
overlap _ _ = False

merge :: Range -> Range -> Range
merge (a,b) (c,d) = (min a c, max b d)

mergecat :: Range -> [Range] -> [Range]
mergecat x [] = [x]
mergecat x (y:ys) | overlap x y = merge x y : ys
mergecat x (y:ys) = x:y:ys

repeatedlyMerge :: [Range] -> [Range]
repeatedlyMerge pairs = ms
  where (_, ms) = until (\(a,b) -> length a == length b) -- not efficient
                        (\(_,b) -> (b, foldr mergecat [] b))
                        ([],pairs)

main :: IO ()
main = do
  contents <- getContents
  args <- System.Environment.getArgs
  let ms = repeatedlyMerge $ sort $ map split $ lines contents in
    if "--initial" `elem` args then
      print $ succ $ snd $ head ms
    else
      -- technically this is wrong, if the blacklist doesn't end with 2^32-1
      print $ sum $ zipWith (\(_,b) (c,_) -> c-(b+1)) ms (tail ms)
