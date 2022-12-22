main = do
  file <- readFile "day2.txt"
  let (op, me) = extract file [] []
  -- let (op', me') = (map translate op, map translate me)
  -- let vals = map val me' 
  -- let outcomes = zipWith outcome op' me'
  -- let part1 = sum outcomes + sum vals
  -- print part1
  let end = me
  let op' = map translate op
  let mine = zipWith figure op' end
  let myVals = map val mine
  let outcomes = map outcomeToInt end

  
  let part2 = sum myVals + sum outcomes
  print part2

extract :: [Char] -> [String] -> [String] -> ([String], [String])
extract "" op me = (op, me)
extract (x : _ : y : _: next) op me
  = extract next (op++[[x]]) (me++[[y]])

-- A B C
-- X Y Z
-- R P S
translate :: String -> String
translate shape
  | shape == "A" || shape == "X" = "R"
  | shape == "B" || shape == "Y" = "P"
  | shape == "C" || shape == "Z" = "S"

-- R P S
-- 1 2 3
val :: String -> Int
val shape
  | shape == "R" = 1
  | shape == "P" = 2
  | shape == "S" = 3



-- L D W
-- 0 3 6
outcome :: String -> String -> Int
-- outcome opponent me

outcome "R" "P" = 6
outcome "P" "S" = 6
outcome "S" "R" = 6

outcome "R" "R" = 3
outcome "P" "P" = 3
outcome "S" "S" = 3

outcome "R" "S" = 0
outcome "P" "R" = 0
outcome "S" "P" = 0

outcomeToInt :: String -> Int
outcomeToInt "X" = 0
outcomeToInt "Y" = 3
outcomeToInt "Z" = 6


-- X = L, Y = D, Z = W
figure :: String -> String -> String
figure "R" "X" = "S"
figure "P" "X" = "R"
figure "S" "X" = "P"

figure "R" "Y" = "R"
figure "P" "Y" = "P"
figure "S" "Y" = "S"

figure "R" "Z" = "P"
figure "P" "Z" = "S"
figure "S" "Z" = "R"
