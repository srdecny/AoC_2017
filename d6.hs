import Data.List
import Data.Maybe
import qualified Data.Set as DSet
import qualified Data.Map as DMap

type Memory = [Integer]
type Dict = DSet.Set Memory
type Dict2 = DMap.Map Memory Integer
size = 16

redistribute :: Memory -> Memory
redistribute memory = let
    (index, amount) = (toInteger $ fromJust $ elemIndex (maximum memory) memory, maximum memory)
    toAll = div amount size
    extras = map (\x -> mod x size) $ [index+1..index + (mod amount size)]
    total (slot, value) = if elem slot extras then value + toAll + 1 else if slot == index then toAll else value + toAll
    in map total $ zip [0..] memory

process :: Integer -> Memory -> Dict -> Integer
process counter memory dict = let
    newMemory = redistribute memory
    newDict = DSet.insert newMemory dict
    in if DSet.member newMemory dict then counter else process (counter + 1) newMemory newDict

process2 :: Integer -> Memory -> Dict2 -> Integer
process2 counter memory map = let
    newMemory = redistribute memory
    newMap = DMap.insert newMemory counter map
    in case DMap.lookup newMemory map of
        (Just counter) -> counter
        Nothing -> process2 (counter + 1) newMemory newMap

callProcess :: Memory -> Integer
callProcess memory = process 1 memory (DSet.empty)

callProcess2 :: Memory -> Integer
callProcess2 memory = process2 1 memory (DMap.empty)

main = do
    content <- readFile "d6.txt"
    let readInts x = read x :: Integer
    print $ callProcess $ map readInts $ words content
    print $ callProcess2 $ map readInts $ words content -- subtract these two
    



