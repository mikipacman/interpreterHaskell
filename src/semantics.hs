module Semantics where
import Data.Map (Map, insert, (!), empty, fromList)
import AbsGramatyka
import ProgramTypes
import Control.Monad.State
 

-- SEMANTIC FUNCTIONS --

semD :: Decl -> DoubleMonad ()
semD decl = case decl of
    FDecl t i p s e -> do
        let new_func var_list = do  -- TODO add recursion and passing arguments
            semB $ Block s
            semE e
        setNewValue i (VFun (Fun new_func))
        return ()
    VDecl t [] -> return ()
    VDecl t (v:vs) -> do
        case t of
            IntT -> case v of
                NoInit i -> setNewValue i (VInt 0)
                Init i e -> do
                    val <- semE e
                    setNewValue i val
            BoolT -> case v of
                NoInit i -> setNewValue i (VBool False)
                Init i e -> do
                    val <- semE e
                    setNewValue i val
            otherwise -> return () -- TODO Add more types and error handling (if needed)
        semD $ VDecl t vs 

semB :: Block -> DoubleMonad ()
semB (Block []) = return ()
semB (Block (s:stmts)) = do
    semS s
    semB (Block stmts)


semS :: Stmt -> DoubleMonad ()
semS stmt = case stmt of
    EmptyStmt -> return ()
    IfStmt e b -> do
        (VBool val) <- semE e
        if val
        then semB b
        else return ()
    IfElseStmt e b1 b2 -> do
        (VBool val) <- semE e
        if val
        then semB b1
        else semB b2
    WhileStmt e b -> let
        loop () = do 
                    (VBool val) <- semE e
                    if val
                    then do
                        semB b
                        loop ()
                    else return ()
        in loop () 
    ForStmt d e a b -> return () -- TODO
    AsStmt (VarAs i e) -> do
        val <- semE e
        setValue i val
    VDecStmt d -> semD d
    ExprStmt e -> do
        semE e
        return ()
    SCostStmt c -> return () -- TODO


semE :: Expr -> DoubleMonad ValueUnion
semE exp = case exp of
    ExprVar i -> getValue i
    ExprLit l -> do 
        case l of 
            IntL i -> return (VInt i)
            -- StringL s -> return (VStr s) -- TODO Add string type
            TrueL -> return (VBool True)
            FalseL -> return (VBool False)
    ExprGC gc -> return (VInt 0)    -- TODO
    ExprBr e -> do
        val <- semE e
        return val
    ExprCall i es -> do
        (VFun (Fun f)) <- getValue i -- TODO Add args
        f []
    ExprAcc i e -> return (VInt 0) -- TODO when arrays are ready
    Neg e -> do
        (VInt val) <- semE e
        return $ VInt (-val)
    Not e -> do
        (VBool val) <- semE e
        return $ VBool (not val)
    EMul e1 op e2 -> do
        let f = case op of
                Mul -> (*)
                Div -> div
                Mod -> mod
        (VInt v1) <- semE e1
        (VInt v2) <- semE e2
        return $ VInt $ f v1 v2 
    EAdd e1 op e2 -> do
        let f = case op of
                Plus -> (+)
                Minus -> (-)
        (VInt v1) <- semE e1
        (VInt v2) <- semE e2
        return $ VInt $ f v1 v2 
    ERel e1 op e2 -> do
        let f = case op of
                Less -> (<)
                LessEq -> (<=)
                Great -> (>)
                GreatEq -> (>=)
                Eq -> (==)
                NotEq -> (/=) 
        v1 <- semE e1               -- TODO this is absolutely disgusting, fix it somehow
        let v11 = case v1 of
                VInt i -> i
                VBool True -> 1
                VBool False -> 0
        v2 <- semE e2
        let v22 = case v2 of
                VInt i -> i
                VBool True -> 1
                VBool False -> 0
        return $ VBool $ f v11 v22
    EAnd e1 e2 -> do
        (VBool v1) <- semE e1
        (VBool v2) <- semE e2
        return $ VBool $ (&&) v1 v2
    EOr e1 e2 -> do
        (VBool v1) <- semE e1
        (VBool v2) <- semE e2
        return $ VBool $ (||) v1 v2


semP :: Program -> DoubleMonad MemoryState
semP (Program (d:ds)) = do
    semD d
    semP (Program ds)
semP (Program []) = do
    (VFun (Fun main)) <- getValue (Ident "main")
    main []
    state <- get
    return state


runTree :: Program -> IO MemoryState
runTree p = evalStateT (semP p) (MemoryState 
    { id_map = fromList [(Ident "print", 0)]       -- TODO move built in functions somewhere else
    , loc_map =  fromList [(0, VFun $ Fun print_func)]
    })