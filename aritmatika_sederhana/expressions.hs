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
