data Expr = C Float | Expr :+ Expr | Expr :- Expr
            | Expr :* Expr | Expr :/ Expr

evaluate (C const) = const
evaluate (expr1 :+ expr2) = (evaluate expr1) + (evaluate expr2)
evaluate (expr1 :- expr2) = (evaluate expr1) - (evaluate expr2)
evaluate (expr1 :* expr2) = (evaluate expr1) * (evaluate expr2)
evaluate (expr1 :/ expr2) = (evaluate expr1) / (evaluate expr2)
