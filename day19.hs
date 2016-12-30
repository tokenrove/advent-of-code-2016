
g 1 k = 0
g n k = (s + k) `rem` n
  where s = g (n - 1) k

h 1 = 0
h n = (s + 1 + (n `div` 2)) `mod` n
  where s = h (n - 1)

test = [1,1,3,3,2] == map ((+)1 . h) [1..5]
