{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TypeFamilies #-}

module MathExpr where
import Debug.Trace (trace)


data MathExpr
    = Symbolic MathExpr -- comes from evaluation
    | N !Float
    | Var !String
    | Neg MathExpr
    | Recip MathExpr
    | Exp MathExpr MathExpr
    | Mul MathExpr MathExpr
    | Add MathExpr MathExpr
    deriving (Eq, Ord)

-- data Expr 
--     = Add' Term Expr
--     | JustTerm Term

-- data Term 
--     = Mul' Pow Term
--     | JustPow Pow

-- data Pow
--     = Pow Atom Pow
--     | JustAtom Atom

-- data Atom 
--     = Num Float
--     | Var' String
--     | Enclosed Expr
--     | Func 

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
Checks if this is an "Atom" MathExpr. Atom here means there is no more 
numerical solving to be done. A True result implies the MathExpr is 
just a number or is a symbolic expression that needs a more powerful 
evaluation function to do more work.  
-}
isAtom :: MathExpr -> Bool
isAtom e = case e of
    Symbolic _  -> True
    N _         -> True
    Var _       -> False -- (Var v) is not Atom, it should be wrapped by a Symbolic first
    Neg _       -> False
    Add _ _     -> False
    Recip _     -> False
    Mul _ _     -> False
    Exp _ _     -> False


eval :: MathExpr -> MathExpr
eval n@(N _)        = n
eval s@(Symbolic _) = eval $ symbolicStep s
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
    Add v1 e2 | isAtom v1              -> Add v1 (evalStep e2)
    Add e1 e2                           -> Add (evalStep e1) e2

    Recip (Symbolic s1)                 -> Symbolic (Recip s1)
    Recip (N n1)                        -> if n1 == 0 
                                            then error "divide by zero" 
                                            else N (1 / n1) -- needs to add divide by 0 support
    Recip e1                            -> Recip (evalStep e1)

    -- vvvv These are abstraction that are alternative to the above vvvv
    -- Add e1 e2                           -> twoArgumentCase Add (+) e1 e2 -- add is explicit to help with readability
    Mul e1 e2                           -> twoArgumentCase Mul (*) e1 e2
    Exp e1 e2                           -> twoArgumentCase Exp (**) e1 e2


twoArgumentCase :: (MathExpr -> MathExpr -> MathExpr)-> (Float -> Float -> Float) -> MathExpr -> MathExpr -> MathExpr
twoArgumentCase mathExprOp _ (Symbolic s1) (Symbolic s2)    = Symbolic (mathExprOp s1 s2)
twoArgumentCase mathExprOp _ (Symbolic s1) n2@(N _)         = Symbolic (mathExprOp s1 n2)
twoArgumentCase mathExprOp _ n1@(N _) (Symbolic s2)         = Symbolic (mathExprOp n1 s2)
twoArgumentCase _ floatOp (N n1) (N n2)                     = N (n1 `floatOp` n2)
twoArgumentCase mathExprOp _ v1 e2 | isAtom v1              = mathExprOp v1 (evalStep e2)
twoArgumentCase mathExprOp _ e1 e2                          = mathExprOp (evalStep e1) e2


-- >>> symbolicStep (Mul (N 2) (Mul (Var "x") (N 2)))
-- Mul (N 2.0) (Mul (N 2.0) (Var "x"))

lengthTerms :: Num p => MathExpr -> p
lengthTerms (Add _ e2) = 1 + lengthTerms e2
lengthTerms _ = 0

sortTerms :: MathExpr -> MathExpr
sortTerms e1 = foldr1 (.) (replicate n bubbleTerms) e1 
    where n = lengthTerms e1

bubbleTerms :: MathExpr -> MathExpr
bubbleTerms (Add e1 (Add e2 e3))
    | e2 < e1   = Add e2 $ bubbleTerms (Add e1 e3)
    | otherwise = Add e1 $ bubbleTerms (Add e2 e3)
bubbleTerms (Add e1 e2)
    | e2 < e1   = Add e2 e1
    | otherwise = Add e1 e2
bubbleTerms e1 = e1

{- |
Does a single symbolic simplification step. The trivial case is seeing a variable:
    >>> symbolicStep $ Symbolic e
    Symbolic e
-}
symbolicStep :: MathExpr -> MathExpr
symbolicStep (Symbolic s) = Symbolic $ symbolicStep s
symbolicStep s = trace (show s) $ case s of
    Add (Var x1) (Var x2) 
        | x1 == x2                  -> Mul (N 2) (Var x1)
    Add v1 v2 
        | all isAtom [v1, v2]       -> Add v2 v1
    Add v1 e2 
        | isAtom v1                 -> Add v1 (symbolicStep e2)
    Add e1 e2                       -> Add (symbolicStep e1) e2
    Mul (Var x1) (Var x2) 
        | x1 == x2                  -> Exp (Var x1) (N 2)
    N n1                            -> N n1
    Var x1                          -> Var x1
--     N n -> N n
--     Var x -> _
--     Neg e1 -> _
--     Add e1 e2 -> _
--     Recip e1 -> _
--     Mul e1 e2 -> _
    _                           -> s

mulOrd :: MathExpr -> MathExpr -> Ordering
N _ `mulOrd` _ = LT
Var x1 `mulOrd` Var x2 = compare x1 x2
mulOrd _ _ = EQ 


{- TODO: Eval step has a symmetry for all one argument and two argument operations !!!! -}

-- numericalAssociatedOperation :: MathExpr -> Float
-- numericalAssociatedOperation (Mul (N a) (N b)) = a * b
-- numericalAssociatedOperation (Add (N a) (N b)) = a * b
-- numericalAssociatedOperation (Recip (N a)) = 1/a
-- numericalAssociatedOperation (Neg (N a)) = -a
-- numericalAssociatedOperation (Neg (N a)) = -a

-- Alternative symbolics function
-- twoArgumentCase :: (MathExpr -> MathExpr -> MathExpr)-> (Float -> Float -> Float) -> MathExpr -> MathExpr -> MathExpr
-- twoArgumentCase mathExprOp _ e1@(Symbolic _) e2@(Symbolic _)    = Symbolic (mathExprOp e1 e2)
-- twoArgumentCase mathExprOp _ e1@(Symbolic _) n2@(N _)         = Symbolic (mathExprOp e1 n2)
-- twoArgumentCase mathExprOp _ n1@(N _) e2@(Symbolic _)         = Symbolic (mathExprOp n1 e2)
-- twoArgumentCase _ floatOp (N n1) (N n2)                     = N (n1 `floatOp` n2)
-- twoArgumentCase mathExprOp _ v1 e2 | isAtom v1             = mathExprOp v1 (evalStep e2)
-- twoArgumentCase mathExprOp _ e1 e2                          = mathExprOp (evalStep e1) e2
