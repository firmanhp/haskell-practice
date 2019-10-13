-- 1.
-- Length function
len' ls = sum (map (\x -> 1) ls)

-- map (+1) (map (+1) xs)
-- sama seperti menambahkan setiap elemen xs dengan 2.
-- map f (map g xs)
-- sama seperti melakukan fungsi f(g(x)) untuk setiap elemen xs.

-- iter n f x = f ( f (... (f x)))
iter :: (Num n, Eq n) => n -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = iter (n-1) f (f x)

-- \n -> iter n succ
-- merupakan anonymous function shg membuat fungsi iter dgn argumen (n), dan fungsi yg digunakan succ.
-- apabila ditambahkan argumen x, (cth: (\n -> iter n succ) n x), artinya mencari successor ke-n dari x
-- karena succ adalah fungsi successor.

-- sum of squares
squaresum n = foldr (+) 0 (map (\x -> x*x) [1..n])

mystery xs = foldr (++) [] (map sing xs)
    where
        sing x = [x]
-- melakukan append utk setiap elemen xs, sehingga menghasilkan list yang sama.

-- (id . f) = id(f(x)) = f(x)
-- (f . id) = f(id(x)) = f(x)
-- (id f) = id(f(x)) = f(x)

-- composeList
composeList :: [(a -> a)] -> (a -> a)
composeList [] = id
composeList (f:fs) = f . composeList fs


-- flip :: (a -> b -> c) -> (b -> a -> c)
fl f a b = f b a

-- [x + 1 | x <- xs] = map (+1) xs
-- [x + y | x <- xs, y <- xs] = map (\(x,y) -> x+y) (zip xs ys)
-- [x + 2 | x <- xs, x > 3] = map (+2) (filter (>3) xs)
-- [x + 3 | (x,_) <- xys] = map (\(x,y) -> x+3) xys
-- [x + 4 | (x,y) <- xys, x+y < 5] = map (\(x,y) -> x+4) (filter (\(x,y) x+y < 5) xys)
-- [x + 5 | Just x <- mxs] = map (\(Just x) -> x) (filter (\x -> x /= Nothing) mxs)

-- map (+3) xs = [x + 3 | x <- xs]
-- filter (>7) xs = [x | x <- xs, x > 7]
-- concat (map (\x -> map (\y -> (x,y)) ys) xs) = [(x, y) | x <- xs, y <- ys]

head' :: [a] -> a
head' xs = case xs of (x:_) -> x
                      (y:_) -> error "asd"
