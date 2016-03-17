romanC :: Char -> Int
romanC c
  | c == 'I' = 1
  | c == 'V' = 5
  | c == 'X' = 10
  | c == 'L' = 50
  | c == 'C' = 100
  | c == 'D' = 500
  | c == 'M' = 1000

f :: Int -> (Int, Int) -> (Int, Int)
f curr (last, acc) =
  (curr, if curr < last then acc - curr else acc + curr)

roman :: String -> Int
roman s = snd $ foldr (f) (0,0) xs
  where xs = map romanC s