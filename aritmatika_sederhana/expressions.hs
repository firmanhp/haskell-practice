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

fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (C const) = fconst const
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :+ expr2) = (fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) expr1) `fadd` (fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) expr2)
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :- expr2) = (fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) expr1) `fsub` (fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) expr2)
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :* expr2) = (fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) expr1) `fmul` (fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) expr2)
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :/ expr2) = (fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) expr1) `fdiv` (fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) expr2)
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (V v) = fvar v
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (Let var expr1 expr2) = flet var expr1 expr2

evaluateFold = fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet)
                where
                    fconst = id
                    fadd = (+)
                    fsub = (-)
                    fmul = (*)
                    fdiv = (/)
                    fvar v = 0
                    flet var expr1 expr2 = evaluateFold (substitute var expr1 expr2)
