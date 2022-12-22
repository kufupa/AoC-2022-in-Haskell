

main = do
  file <- readFile "day1.txt"
  let b = toList2 (toList file "") []
  let ints = map (map (read::String->Int)) b
  print (foldr max 0 (map sum ints))
  -- part 2
  let cals = map sum ints
  let sorted = quicksort cals
  let top3 = take 3 (reverse sorted)
  let part2 = sum top3
  print part2


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) 
  = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- part1 :: String -> Int
-- part1 file = 

  -- l = init l : last l
toList :: String -> String -> [String]
toList [] s = [s]
toList (x:xs) s
  | x == '\n' = s : toList xs ""
  | otherwise = toList xs (s ++ [x])

toList2 :: [String] -> [String] -> [[String]]
toList2 [] s = [s]
toList2 (x:xs) s
  | x == "" = s : toList2 xs []
  | otherwise = toList2 xs (s ++ [x])

-- conv to list of lists
-- foldr list
-- compare for largest

-- Add all ints till newline
-- Add to list or comp to prev?

