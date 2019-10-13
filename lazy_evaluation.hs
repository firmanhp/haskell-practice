-- 1. [x + y | x <- [1 .. 4], y <- [2 .. 4], x > y]
-- Langkah evaluasi, sama seperti loop python berikut
-- for x in [1, 2, 3, 4]:
--   for y in [2, 3, 4]:
--     if x > y:
--       yield x+y
-- yang artinya akan yield utk pasangan (x,y) berikut: (3,2), (4,2), dan (4,3).

-- 2. divisor n
divisor n = [x | x <- [1..n], n `mod` x == 0]

-- 3. quicksort dengan list comprehension
quicksort [] = []
quicksort (pivot:ls) = (quicksort [x | x <- ls, x <= pivot]) ++ [pivot] ++ (quicksort [x | x <- ls, x > pivot])

-- 4. definisi infinite list untuk permutation
-- Asumsi: semua elemen dalam list unik
permutation [] = [[]]
permutation ls = [(x:l) | x <- ls, l <- permutation [y | y <- ls, y /= x]]

-- 5. sieve of erathosthenes
prime = prime' [2..]
    where prime' (p:ls) = p : prime' [x | x <- ls, x `mod` p /= 0]

-- 6. triple pythagoras
pythaTriple = [(x, y, z) | z <- [1..], y <- [1..z], x <- [1..y], (x*x) + (y*y) == (z*z)]