data Op = Add | Sub | Mul | Div

apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool 
valid Add x y = x <= y -- improves efficiency
valid Sub x y = x > y
valid Mul x y = x <= y && x != 1 && y != 1 -- improves efficiency
valid Div x y = x `mod` y == 0 && y != 1 -- improves efficiency

data Expr = Val Int | App Op Expr Expr

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l
                                , y <- eval r
                                , valid o x y]

-- TODO
-- Makes all possible groupings of variables in a 2d list
choices :: [a] -> [[a]]


values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- TODO splits it up! Use take and drop
split :: [a] -> [([a], [a])]

exprs :: [Int] -> [Expr]
exprs []   = []
exprs [n]  = [Val n]
exprs [ns] = [e | (ls, rs) <- split ns
                , l        <- exprs ls
                , r        <- exprs rs
                , e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

solutions :: [Int] -> Int -> Expr
solutions ns n = [e | ns' <- choices ns
                    , e   <- exprs ns'
                    , eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns
                    , lx      <- results ls
                    , ry      <- results rs
                    , res     <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- [Add, Sub, Mul, Div]
                                                 , valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns'   <- choices ns
                     , (e,m) <- results ns'
                     , m == n]

