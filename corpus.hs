import System.IO
import Control.Monad
import Data.List.Split
import qualified Data.SetMap as Map


corpusPath :: String
corpusPath = "browntag_nolines.txt"

corpus :: IO String
corpus = join $ fmap hGetContents $ openFile corpusPath ReadMode

tagMap :: [String] -> Map.SetMap String String
tagMap []     = Map.empty
tagMap (x:xs) = Map.insert p w $ tagMap xs
  where pw = splitOn "_" x
        w = head pw
        p = head $ tail pw
