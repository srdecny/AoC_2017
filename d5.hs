import qualified Data.Map as Dmap
import Data.Maybe

type Offset = Integer
type Index = Integer

type Dict = Dmap.Map Index Offset

loadDict :: [String] -> Dict
loadDict lines = let
    numbers = zip [0..] $ map read lines
    in Dmap.fromList numbers

step :: Integer -> Offset -> Dict -> Integer
step counter pointer dict = let
    jump = Dmap.lookup pointer dict
    in if isJust jump then let
        offset = fromJust jump
        newDict = Dmap.insert pointer (offset + 1) dict
        in step (counter + 1) (pointer + offset) newDict
        else counter

step2 :: Integer -> Offset -> Dict -> Integer
step2 counter pointer dict = let
    jump = Dmap.lookup pointer dict
    in if isJust jump then let
        offset = fromJust jump
        newOffset = if offset >= 3 then offset - 1 else offset + 1
        newDict = Dmap.insert pointer (newOffset) dict
        in step2 (counter + 1) (pointer + offset) newDict
        else counter

main = do
    content <- readFile "d5.txt"
    print $ step 0 0 (loadDict $ lines content)
    print $ step2 0 0 (loadDict $ lines content) -- compile with -O2 flag for tail recursion optimization