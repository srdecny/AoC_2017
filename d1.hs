import Data.Char

filterPairs :: [(Char, Char)] -> [Int]
filterPairs pairs = map (\x -> digitToInt x) $ map (\x -> fst x) $ filter (\(x,y) -> x == y) pairs

captcha :: [Char] -> Int
captcha number = let
    pairs = (last number, head number): zip number (tail number)
    in sum $ filterPairs pairs

captcha2 :: [Char] -> Int
captcha2 number = let
    doubledNumber = concat $ replicate 2 number
    len = length number
    distance = quot len 2
    pairs = [(doubledNumber !! index, doubledNumber !! (index + distance)) | index <- [0..len]]
    in sum $ filterPairs pairs    

main = do
    number <- readFile "d1.txt"
    print $ captcha number
    print $ captcha2 number