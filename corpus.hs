import System.IO
import System.IO.Unsafe -- shh
import Control.Monad
import System.Random.Pick
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.SetMap as Map


corpusPath :: String
corpusPath = "browntag_nolines.txt"

corpus :: String
corpus = unsafePerformIO $ join $ fmap hGetContents $ openFile corpusPath ReadMode
-- unsafe because i need to memoize it and also don't want fmap everywhere

word, part :: String -> String
word =        takeWhile (/='_')
part = tail . dropWhile (/='_')

partMap :: Map.SetMap String String
partMap = (m . words) corpus where
  m []     = Map.empty
  m (x:xs) = Map.insert (part x) (word x) $ m xs

parts :: [String]
parts = Map.keys partMap

getWords :: String -> [String]
getWords x = Set.elems $ Map.lookup x partMap

getWord :: String -> IO String
getWord = pickOne . getWords

sub :: String -> IO String
sub = getWord . part

test = do
  x <- pickOne $ lines $ corpus
  return $ intercalate " " $ map (unsafePerformIO . sub) $ words x
-- bad
