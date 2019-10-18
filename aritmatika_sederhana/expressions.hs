import Control.Applicative

data Expr = C Float | Expr :+ Expr | Expr :- Expr
            | Expr :* Expr | Expr :/ Expr
            | V String | Let String Expr Expr

-- Substitute
substitute var expr (V var2)
    | var == var2   = expr
    | otherwise     = V var2
substitute var expr (C const) = C const
substitute var expr (expr1 :+ expr2) = (substitute var expr expr1) :+ (substitute var expr expr2)
substitute var expr (expr1 :- expr2) = (substitute var expr expr1) :- (substitute var expr expr2)
substitute var expr (expr1 :* expr2) = (substitute var expr expr1) :* (substitute var expr expr2)
substitute var expr (expr1 :/ expr2) = (substitute var expr expr1) :/ (substitute var expr expr2)
substitute var expr (Let var2 expr2 expr3) = Let var2 expr2 (substitute var expr expr3)


evaluate (C const) = const
evaluate (expr1 :+ expr2) = (evaluate expr1) + (evaluate expr2)
evaluate (expr1 :- expr2) = (evaluate expr1) - (evaluate expr2)
evaluate (expr1 :* expr2) = (evaluate expr1) * (evaluate expr2)
evaluate (expr1 :/ expr2) = (evaluate expr1) / (evaluate expr2)
evaluate (Let v v_expr expr) = evaluate (substitute v v_expr expr)

foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (C const) = fconst const
foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :+ expr2) =  let fs = (fconst,fadd,fsub,fmul,fdiv,fvar,flet) in
                                                                    fadd (foldExpr fs expr1) (foldExpr fs expr2)
foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :- expr2) =  let fs = (fconst,fadd,fsub,fmul,fdiv,fvar,flet) in
                                                                    fsub (foldExpr fs expr1) (foldExpr fs expr2)
foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :* expr2) =  let fs = (fconst,fadd,fsub,fmul,fdiv,fvar,flet) in
                                                                    fmul (foldExpr fs expr1) (foldExpr fs expr2)
foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :/ expr2) =  let fs = (fconst,fadd,fsub,fmul,fdiv,fvar,flet) in
                                                                    fdiv (foldExpr fs expr1) (foldExpr fs expr2)
foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (V v) = fvar v
foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (Let var expr1 expr2) = flet var expr1 expr2


-- Menambahkan fungsi fold membuka banyak kemungkinan dalam memanipulasi pohon ekspresi.
-- Salah satunya, fungsi evaluasi di bawah memanfaatkan fungsi fold yang telah dibuat,
-- yang mana fungsi setiap operasinya disesuaikan.
evaluateFold = foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet)
                where
                    fconst = id
                    fadd = (+)
                    fsub = (-)
                    fmul = (*)
                    fdiv = (/)
                    fvar v = 0
                    flet var expr1 expr2 = evaluateFold (substitute var expr1 expr2)

-- Menghitung banyaknya konstanta yang digunakan (yang tentu saja menggunakan fold)
countConstants =    foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet)
                    where
                        fconst c = 1
                        fadd = (+)
                        fsub = (+)
                        fmul = (+)
                        fdiv = (+)
                        fvar v =  1
                        flet _ _ = countConstants

-- Menghitung banyaknya referensi variabel yang ada
countVariables =    foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet)
                    where
                        fconst c = 0
                        fadd = (+)
                        fsub = (+)
                        fmul = (+)
                        fdiv = (+)
                        fvar v = 1
                        flet _ _ expr = countVariables expr

-- Menghitung banyaknya operator yang ada,
-- sama seperti menghitung banyaknya non-leaf node pada expression tree.
countOperators =    foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet)
                    where
                        fconst c = 0
                        fadd = (\x y -> x + y + 1)
                        fsub = (\x y -> x + y + 1)
                        fmul = (\x y -> x + y + 1)
                        fdiv = (\x y -> x + y + 1)
                        fvar v = 0
                        flet _ _ expr = (countOperators expr) + 1

-- Pengecekan apakah terdapat division by zero dalam evaluasi ekspresi.
-- Fungsi di bawah dibuat dengan pertimbangan apakah dalam kedepannya akan muncul error baru selain
-- DivisionByZero (seperti NegativeSquareRoot).
data EvaluationErrorReason = DivByZero | NegativeSquareRoot
                        deriving (Show)
data CheckedEvaluation evaluationType = EvaluationOk evaluationType | EvaluationError EvaluationErrorReason
                        deriving (Show)

instance Functor CheckedEvaluation where
    fmap f (EvaluationError reason) = (EvaluationError reason)
    fmap f (EvaluationOk x) = EvaluationOk (f x)

instance Applicative CheckedEvaluation where
    pure f = EvaluationOk f
    (EvaluationError reason) <*> _ = (EvaluationError reason)
    (EvaluationOk f) <*> something = fmap f something

checkedDivision (EvaluationOk x) (EvaluationOk y)
    | y == 0.0  = EvaluationError DivByZero
    | otherwise = EvaluationOk (x / y)
checkedDivision (EvaluationError reason) _ = (EvaluationError reason)

evaluateAndCheck =  foldExpr (fconst,fadd,fsub,fmul,fdiv,fvar,flet)
                    where
                        fconst c = EvaluationOk c
                        fadd x y = (+) <$> x <*> y
                        fsub x y = (-) <$> x <*> y
                        fmul x y = (*) <$> x <*> y
                        fdiv = checkedDivision
                        fvar v = EvaluationOk 0.0
                        flet var expr1 expr2 = evaluateAndCheck (substitute var expr1 expr2)

isDivisionByZero expr =
    case evaluateAndCheck expr of
        (EvaluationOk _) -> False
        (EvaluationError DivByZero) -> True
        (EvaluationError _) -> False