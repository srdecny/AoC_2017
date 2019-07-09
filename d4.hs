import Data.List

hasDuplicates :: String -> Bool
hasDuplicates line = length (nub ws) /= length ws where ws = words line

hasAnagrams :: String -> Bool
hasAnagrams line = length (nub ws) /= length ws where ws = map Data.List.sort $ words line



main = do
    content <- readFile "d4.txt"
    print $ length $ filter (==False) $ map hasDuplicates $ lines content
    print $ length $ filter (==False) $ map hasAnagrams $ lines content
