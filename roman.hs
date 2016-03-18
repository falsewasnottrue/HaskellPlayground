import qualified Data.Map as M

c2r = M.fromList [('I',1),('V',5),('X',10),('L',50),('C',100),('D',500),('M',1000)]

f :: Int -> (Int, Int) -> (Int, Int)
f curr (last, acc) =
  (curr, if curr < last then acc - curr else acc + curr)

roman :: String -> Int
roman s = snd $ foldr f (0,0) $ map (c2r M.!) s
