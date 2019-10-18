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
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :+ expr2) =  let fs = (fconst,fadd,fsub,fmul,fdiv,fvar,flet) in
                                                                fadd (fold fs expr1) (fold fs expr2)
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :- expr2) =  let fs = (fconst,fadd,fsub,fmul,fdiv,fvar,flet) in
                                                                fsub (fold fs expr1) (fold fs expr2)
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :* expr2) =  let fs = (fconst,fadd,fsub,fmul,fdiv,fvar,flet) in
                                                                fmul (fold fs expr1) (fold fs expr2)
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (expr1 :/ expr2) =  let fs = (fconst,fadd,fsub,fmul,fdiv,fvar,flet) in
                                                                fdiv (fold fs expr1) (fold fs expr2)
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (V v) = fvar v
fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet) (Let var expr1 expr2) = flet var expr1 expr2


-- Menambahkan fungsi fold membuka banyak kemungkinan dalam memanipulasi pohon ekspresi.
-- Salah satunya, fungsi evaluasi di bawah memanfaatkan fungsi fold yang telah dibuat,
-- yang mana fungsi setiap operasinya disesuaikan.
evaluateFold = fold (fconst,fadd,fsub,fmul,fdiv,fvar,flet)
                where
                    fconst = id
                    fadd = (+)
                    fsub = (-)
                    fmul = (*)
                    fdiv = (/)
                    fvar v = 0
                    flet var expr1 expr2 = evaluateFold (substitute var expr1 expr2)
