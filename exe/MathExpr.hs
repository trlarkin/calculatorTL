{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module MathExpr where

data MathExpr
    = Symbolic MathExpr -- comes from evaluation
    | N Float
    | Var String
    | Neg MathExpr
    | Add MathExpr MathExpr
    | Recip MathExpr
    | Mul MathExpr MathExpr
    | Exp MathExpr MathExpr

instance Show MathExpr where
    show :: MathExpr -> String
    show e = case e of
        Symbolic s              -> show s
        N n                     -> if n == (fromIntegral . round) n then show (truncate n) else show n
        Var x               -> x
        Neg u | isUnit u        -> "-" ++ show u
        Neg e1                  -> "-" ++ "(" ++ show e1 ++ ")"
        Add e1 (Neg e2)         -> show e1 ++ " - " ++ show e2
        Add e1 e2               -> show e1 ++ " + " ++ show e2
        Recip u | isUnit u      -> "1/" ++ show u
        Recip e1                -> "1/(" ++ show e1 ++ ")"
        Mul e1 (Recip e2)       -> show e1 ++ " / " ++ show e2
        Mul (Var x1) e2         -> x1 ++ show e2
        Mul e1 (Var x2)         -> "(" ++ show e1 ++ ")" ++ x2
        Mul e1 e2               -> "(" ++ show e1 ++ ")" ++ show e2
        Exp u1 u2 | all isUnit [u1, u2]          
                                -> show u1 ++ "^" ++ show u2 
        Exp u1 e2 | isUnit u1   -> show u1 ++ "^" ++ "(" ++ show e2 ++ ")"
        Exp e1 u2 | isUnit u2   -> "(" ++ show e1 ++ ")" ++ "^" ++ show u2
        Exp e1 e2               -> "(" ++ show e1 ++ ")" ++ "^" ++ "(" ++ show e2 ++ ")"
        

-- vvvvvvvvvv Show tests vvvvvvvvvv

-- >>> show $ Neg (Var "x")
-- "-x"

-- >>> show $ Add (Var "x") (Var "y") 
-- "x + y"

-- >>> show $ Add (Var "x") (Neg (N 3))
-- "x - 3"

-- >>> show $ Add (Var "x") (Mul (N 4) (Var "y"))
-- "x + 4y"

-- >>> show $ Mul (Mul (Var "w") (Recip $ Var "y")) (Var "x")
-- "(w / y)x"

-- >>> show $ Neg (Mul (Add (Var "x") (N 4)) (Var "y"))
-- "-((x + 4)y)"

-- ^^^^^^^^^^ Show tests ^^^^^^^^^^

{- |
This function is for printing. A "Unit" is defined as a MathExpr 
that is visually identical in any situation with or without parentheses.
-}
isUnit :: MathExpr -> Bool
isUnit e = case e of
    Symbolic _    -> False
    N _           -> True
    Var _         -> True
    Neg _         -> False -- funky case, will err on side of not unit
    Add _ _       -> False -- x * y + z /= x * (y + z)
    Recip _       -> False -- 1/1/x /= 1/(1/x)
    Mul _ _       -> False -- 1/x * y /= 1/(x * y)
    Exp _ _       -> False -- unproven TODO


{- |
Checks if this is a "basic" MathExpr. Basic here means there is no more 
numerical solving to be done. A True result implies the MathExpr is 
just a number or is a symbolic expression that needs a more powerful 
evaluation function to do more work.  
-}
isBasic :: MathExpr -> Bool
isBasic e = case e of
    Symbolic _  -> True
    N _         -> True
    Var _       -> False -- (Var v) is not basic, it should be wrapped by a Symbolic first
    Neg _       -> False
    Add _ _     -> False
    Recip _     -> False
    Mul _ _     -> False
    Exp _ _     -> False


eval :: MathExpr -> MathExpr
eval s@(Symbolic _)   = s
eval n@(N _)          = n
eval e              = eval $ evalStep e

{- |
Evaluates the numerical parts of a MathExpr. When it hits a node of the parse tree that has been 
determined to be symbolic in nature, it will ignore that subtree. It will either return a number 
or a symbolic expression. Symbolic expressions come with no promise of evaluation. 
-}
evalStep :: MathExpr -> MathExpr
evalStep e = case e of
    Symbolic _                          -> error "evalStep should never be called on a (Symbolic v) instance"
    N n                                 -> N n
    Var x                               -> Symbolic $ Var x

    Neg (Symbolic s1)                    -> Symbolic $ Neg s1
    Neg (N n1)                          -> N (- n1)
    Neg e1                              -> Neg (evalStep e1)

    Add (Symbolic s1) (Symbolic s2)     -> Symbolic (Add s1 s2)
    Add (N n1) (N n2)                   -> N (n1 + n2)
    Add (Symbolic s1) n2@(N _)          -> Symbolic (Add s1 n2)
    Add n1@(N _) (Symbolic s2)          -> Symbolic (Add n1 s2)
    Add v1 e2 | isBasic v1              -> Add v1 (evalStep e2)
    Add e1 e2                           -> Add (evalStep e1) e2

    Recip (Symbolic s1)                 -> Symbolic (Recip s1)
    Recip (N n1)                        -> if n1 == 0 
                                            then error "divide by zero" 
                                            else N (1 / n1) -- needs to add divide by 0 support
    Recip e1                            -> Recip (evalStep e1)

    Mul (Symbolic s1) (Symbolic s2)     -> Symbolic (Mul s1 s2)
    Mul (Symbolic s1) n2@(N _)          -> Symbolic (Mul s1 n2)
    Mul n1@(N _) (Symbolic s2)          -> Symbolic (Mul n1 s2)
    Mul (N n1) (N n2)                   -> N (n1 * n2)
    Mul v1 e2 | isBasic v1              -> Mul v1 (evalStep e2)
    Mul e1 e2                           -> Mul (evalStep e1) e2

    Exp (Symbolic s1) (Symbolic s2)     -> Symbolic (Exp s1 s2)
    Exp (Symbolic s1) n2@(N _)          -> Symbolic (Exp s1 n2)
    Exp n1@(N _) (Symbolic s2)          -> Symbolic (Exp n1 s2)
    Exp (N n1) (N n2)                   -> N (n1 ** n2)
    Exp v1 e2 | isBasic v1              -> Exp v1 (evalStep e2)
    Exp e1 e2                           -> Exp (evalStep e1) e2

{- |
Does a single symbolic simplification step. The trivial case is simply 
unwrapping a doubled up symbolic:
    >>> symbolicStep $ Symbolic (Symbolic e)
    Symbolic e
-}
symbolicStep :: MathExpr -> MathExpr
-- symbolicStep (Symbolic s) = case s of
--     Symbolic s1 -> Symbolic s1
--     N n -> N n
--     Var x -> _
--     Neg e1 -> _
--     Add e1 e2 -> _
--     Recip e1 -> _
--     Mul e1 e2 -> _
symbolicStep _ = error "cannot perform symbolicStep on non-symbolic expr"

{- TODO: Eval step has a symmetry for all one argument and two argument operations !!!! -}

-- numericalAssociatedOperation :: MathExpr -> Float
-- numericalAssociatedOperation (Mul (N a) (N b)) = a * b
-- numericalAssociatedOperation (Add (N a) (N b)) = a * b
-- numericalAssociatedOperation (Recip (N a)) = 1/a
-- numericalAssociatedOperation (Neg (N a)) = -a
-- numericalAssociatedOperation (Neg (N a)) = -a

-- twoArgumentCase :: (MathExpr -> MathExpr -> MathExpr) -> MathExpr -> MathExpr -> MathExpr
-- twoArgumentCase op (Symbolic s1) (Symbolic s2) = Symbolic (op s1 s2)
-- twoArgumentCase op (Symbolic s1) n2@(N _)      = Symbolic (op s1 n2)
-- twoArgumentCase op n1@(N _) (Symbolic s2)      = Symbolic (op n1 s2)
-- twoArgumentCase op (N n1) (N n2)               = N (numericalAssociatedOperation (op n1 n2))
-- twoArgumentCase op v1 e2 | isBasic v1          = op v1 (evalStep e2)
-- twoArgumentCase op e1 e2                       = op (evalStep e1) e2

