checksum :: [String] -> Integer
checksum rows = let
    diff l = ((maximum row) - (minimum row)) where row = map read $ words l
    in sum $ map diff rows

checksum2 :: [String] -> Integer
checksum2 rows = let
    divide l = let
        row = map read $ words l :: [Integer]
        in maximum $ map (\x -> maximum(map (\y -> if mod x y == 0 then div x y; else 0) row)) row
    in sum $ map divide rows


main = do
    content <- readFile "d2.txt"
    let rows = (map . map) (\x -> if x == '\t' then ' '; else x) $ lines content
    print $ checksum rows
    print $ checksum2 rows
    
    -- 394 high