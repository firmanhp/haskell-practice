> module Trees where

-- This code was automatically extracted from a .lhs file that
-- uses the following convention:

-- lines beginning with ">" are executable
-- lines beginning with "<" are in the text,
     but not necessarily executable
-- lines beginning with "|" are also in the text,
     but are often just expressions or code fragments.

> data Shape = Rectangle Side Side
>            | Ellipse Radius Radius
>            | RtTriangle Side Side
>            | Polygon [Vertex]
>      deriving Show
>
> type Radius = Float
> type Side   = Float
> type Vertex  = (Float,Float)

> data List a = Nil | MkList a (List a)

< Nil    :: List a
< MkList :: a -> List a -> List a

< []  :: [a]
< (:) :: a -> [a] -> [a]

> data Tree a = Leaf a | Branch (Tree a) (Tree a)

< data IntegerTree = Leaf Integer | Branch IntegerTree IntegerTree
< data SimpleTree  = Leaf | Branch SimpleTree SimpleTree

> data InternalTree a = ILeaf 
>                     | IBranch a (InternalTree a) (InternalTree a)

> data FancyTree a b = FLeaf a 
>                    | FBranch b (FancyTree a b) (FancyTree a b)

> mapTree :: (a->b) -> Tree a -> Tree b

> mapTree f (Leaf x)       = Leaf (f x)
> mapTree f (Branch t1 t2) = Branch (mapTree f t1) 
>                                    (mapTree f t2)

> fringe               :: Tree a -> [a]
> fringe (Leaf x)       = [x]
> fringe (Branch t1 t2) = fringe t1 ++ fringe t2

> treeSize               :: Tree a -> Integer
> treeSize (Leaf _)       = 1
> treeSize (Branch t1 t2) = treeSize t1 + treeSize t2

> treeHeight           :: Tree a -> Integer
> treeHeight (Leaf _)       = 0
> treeHeight (Branch t1 t2) = 1 + max (treeHeight t1) 
>                                     (treeHeight t2)

< data Expr = C Float | Add Expr Expr | Sub Expr Expr
<           | Mul Expr Expr | Div Expr Expr

> data Expr = C Float | Expr :+ Expr | Expr :- Expr
>           | Expr :* Expr | Expr :/ Expr 
>           | V [Char]
>           | Let String Expr Expr      
>      deriving Show


-- >>> (C 10.0 :+ (C 8 :/ C 2)) :* (C 7 :- C 4)
-- (C 10.0 :+ (C 8.0 :/ C 2.0)) :* (C 7.0 :- C 4.0)
--

> subst :: String -> Expr -> Expr -> Expr

> subst v0 e0 (V v1)         = if (v0 == v1) then e0 else (V v1)
> subst v0 e0 (C c)          = (C c)
> subst v0 e0 (e1 :+ e2)     = subst v0 e0 e1 :+ subst v0 e0 e2
> subst v0 e0 (e1 :- e2)     = subst v0 e0 e1 :+ subst v0 e0 e2
> subst v0 e0 (e1 :* e2)     = subst v0 e0 e1 :+ subst v0 e0 e2
> subst v0 e0 (e1 :/ e2)     = subst v0 e0 e1 :+ subst v0 e0 e2
> subst v0 e0 (Let v1 e1 e2) = Let v1 e1 (subst v0 e0 e2)

> evaluate :: Expr -> Float

> evaluate (C x) = x
> evaluate (e1 :+ e2)    = evaluate e1 + evaluate e2
> evaluate (e1 :- e2)    = evaluate e1 - evaluate e2
> evaluate (e1 :* e2)    = evaluate e1 * evaluate e2
> evaluate (e1 :/ e2)    = evaluate e1 / evaluate e2
> evaluate (Let v e0 e1) = evaluate (subst v e0 e1)
> evaluate (V v)         = 0.0   

< data InternalTree a = ILeaf 
<                     | IBranch a (InternalTree a) (InternalTree a)

< takeTree      :: Int -> InternalTree a -> InternalTree a
< takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a

< t = let t' = IBranch 1 ILeaf ILeaf
<     in Branch 2 t' t'

-- >>> exp0 = (((C 5.0) :+ (C 7) :* (C 4)))
-- >>> exp0
-- (C 5.0 :+ C 7.0) :* C 4.0
--


-- >>> evaluate exp0
-- 48.0
--


-- >>> evaluate (((C 5) :+ (C 7) :* (C 4)))
-- 48.0
--
-- >>> exp1 = Let "x" (C 7) ((C 5) :+ (C 7) :* (V "x"))
-- >>> exp1
-- >>> evaluate exp1
-- Let "x" (C 7.0) ((C 5.0 :+ C 7.0) :* V "x")
-- 19.0
--

-- >>> evaluate exp1
-- 19.0
--


-- >>> evaluate (Let "x" (C 5) (V "x" :+ (C 7)))
-- 12.0
--

> simple n a b = n * (a-b)

-- >>> fun1 = simple 5 2
-- >>> fun1 3
-- 25
--
-- >>> fun1 7
-- 45
--

> myflip f x y = f y x

> newSimple = myflip simple

> newSimple2 n = myflip (simple n) 

-- >>> :t newSimple
-- newSimple :: Integer -> Integer -> Integer -> Integer
--

-- >>> newSimple 5 2 3
-- 4

-- >>> simple 5 2 3
-- -5
--

-- >>> newSimple2 5 2 3
-- 5
--

-- >>> simple 5 2 3
-- >>> fun0 = myflip simple n a
-- -5
-- <BLANKLINE>
-- <interactive>:1230:23: error: Variable not in scope: n
-- <BLANKLINE>
-- <interactive>:1230:25: error: Variable not in scope: a
--

