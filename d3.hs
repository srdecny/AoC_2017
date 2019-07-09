import Data.List
import qualified Data.Map as Dmap

type Dict = Dmap.Map (Integer, Integer) Integer

distance :: Integer -> Integer
distance number = let
    diagonal = map (\x -> (4*x^2-4*x+1, x)) [0..]
    (nearestCorner, level) = head $ filter (\x -> fst x >= number) diagonal
    corners = map (\x -> nearestCorner - 2*x*level) [0,1,2,3]
    offset = minimum $ map (\x -> abs (x - number)) corners
    in 2*(level-1) - offset 
    
spiral :: [(Integer, Integer)]
spiral = let
    generate (a,b) = let
        x = a + 1
        in (a,b) : [(x,j) | j <- [b..x]] ++ (reverse [(c,x) | c <-[(-x)..x-1]]) ++ reverse ([(-x, d) | d <- [(-x)..x-1]]) ++ [(e, (-x))|e <- [(-x+1)..x-1]]
    corners = map (\x -> (x, (-x))) [0..]
    in concat $ map generate corners

adjacent :: (Integer, Integer) -> [(Integer, Integer)]
adjacent (x, y) = [(a, b)| a <- [x-1..x+1], b <- [y-1..y+1]]

process :: (Num a) => Maybe a -> a
process (Just a) = a
process Nothing = 0

square :: Integer -> [(Integer, Integer)] -> Dict -> Integer
square number list dict = let
    searched = head list
    value = traceShowId $ sum $ map process $ map (\x -> (Dmap.lookup x dict)) $ adjacent searched
    newDict = Dmap.insert searched value dict
    in if value > number then value else square number (tail list) newDict

findsquare :: Integer -> Integer
findsquare number = square number spiral (Dmap.singleton (0,0) 1 )

main = do
    print $ distance 265149
    print $ findsquare 265149
