module Semantics where
import Data.Map (Map, insert, (!), empty, fromList, member)
import AbsGramatyka
import ProgramTypes
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

-- SEMANTIC FUNCTIONS --


semManyD :: [Decl] -> DoubleMonad Env
semManyD [] = ask
semManyD (d:ds) = do
    env <- semD d
    env2 <- local (const env) $ semManyD ds
    return env2


semD :: Decl -> DoubleMonad Env
semD decl = case decl of
    FDecl t i p d s e -> do
        env <- ask
        let new_func val_list = do
            if (length p) == (length val_list)
            then do
                let decl_list = map mapToDecl $ zip p val_list
                env2 <- local (const env) $ semManyD decl_list   -- set params
                env3 <- local (const env2) $ setNewValue i (VFun (Fun new_func)) -- recursion 
                env4 <- local (const env3) $ semManyD d          -- init declared vars
                local (const env4) $ semB $ Block s
                local (const env4) $ semE e
            else do
                let (Ident fun_name) = i
                throwError $ "Wrong number of arguments in " ++ fun_name ++ " call!"
        new_env <- setNewValue i (VFun (Fun new_func))
        return new_env
    VDecl t [] -> ask
    VDecl t (v:vs) -> do
        env <- case t of
            IntT -> case v of
                NoInit i -> do
                    setNewValue i (VInt 0)
                Init i e -> do
                    val <- semE e
                    setNewValue i val
            BoolT -> case v of
                NoInit i -> setNewValue i (VBool False)
                Init i e -> do
                    val <- semE e
                    setNewValue i val
            StringT -> case v of 
                NoInit i -> setNewValue i (VStr "")
                Init i e -> do
                    val <- semE e
                    setNewValue i val
        local (const env) $ semD $ VDecl t vs 
    ADecl t [] -> ask
    ADecl t (v:vs) -> do
        env <- case t of 
            (ArrT b []) -> case v of
                Init i e -> throwError "Initialization of arrays is not supported yet."
                NoInit i -> do
                    semD $ VDecl b [NoInit i]
            (ArrT b ((Acc e1):as)) -> case v of
                Init i e -> throwError "Initialization of arrays is not supported yet."
                NoInit i -> do
                    (VInt v) <- semE e1
                    let v_int = fromInteger v
                    arr <- mapType i (replicate v_int $ ArrT b as)
                    let m = zip [0..] arr
                    setNewValue i (VArr $ fromList m)
        local (const env) $ semD $ ADecl t vs 


mapType :: Ident -> [ArrType] -> DoubleMonad [Location]     -- TODO make it prettier
mapType i [] = return []
mapType i (t:ts) = do
    let decl = ADecl t [NoInit i]
    env <- semD decl
    l <- local (const env) $ getLoc i
    ls <- mapType i ts
    return ([l] ++ ls)


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
    ForStmt d es as b -> do 
        env <- semD d
        let step = Block $ map (\x -> AsStmt x) as
        local (const env) $ loop step
        where 
            loop step = do
                    vs <- semManyE es
                    let cont = all (== True) $ map (\(VBool v) -> v) vs
                    if cont
                    then do
                        semB b
                        semB step
                        loop step
                    else return ()
    AsStmt a -> case a of
        (VarAs i a e) -> do
            v <- semE e
            l <- getLoc i
            l2 <- getArrLoc l a
            setLoc l2 v
    ExprStmt e -> do
        semE e
        return ()
    SCostStmt c -> return () -- TODO


semManyE :: [Expr] -> DoubleMonad [ValueUnion]
semManyE [] = return []
semManyE (e:es) = do
    v <- semE e
    vs <- semManyE es 
    return (v:vs) 


semE :: Expr -> DoubleMonad ValueUnion
semE exp = case exp of
    ExprLit l -> do 
        case l of 
            IntL i -> return (VInt i)
            StringL s -> return (VStr s)
            TrueL -> return (VBool True)
            FalseL -> return (VBool False)
    ExprGC gc -> return (VInt 0)    -- TODO
    ExprBr e -> do
        val <- semE e
        return val
    ExprCall i es -> do
        (VFun (Fun f)) <- getValue i
        vs <- semManyE es
        f vs
    ExprAcc i a -> do
        l <- getLoc i
        l2 <- getArrLoc l a
        v <- getLocValue l2
        return v
    Neg e -> do
        (VInt val) <- semE e
        return $ VInt (-val)
    Not e -> do
        (VBool val) <- semE e
        return $ VBool (not val)
    EMul e1 op e2 -> do
        (VInt v1) <- semE e1
        (VInt v2) <- semE e2
        let f = case op of
                Mul -> (*)
                Div -> div 
                Mod -> mod
        if op == Div && v2 == 0
        then throwError "Divide by zero!"
        else return $ VInt $ f v1 v2 
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
    env <- semD d
    local (const env) $ semP (Program ds)
semP (Program []) = do
    (VFun (Fun main)) <- getValue (Ident "main")
    env <- ask
    local (const env) $ main []
    state <- get
    return state


runTree :: Program -> IO ()
runTree p = do 
    r <- runExceptT (runReaderT (evalStateT (semP p) get_init_memory_state) get_init_env)
    reportResult r

reportResult :: Either String MemoryState -> IO ()
reportResult (Right mem) = putStrLn ("[Success]\n\n" ++ (show mem))
reportResult (Left e) = putStrLn ("Exception occured: " ++ (show e))


getArrLoc :: Location -> [Acc] -> DoubleMonad Location
getArrLoc l [] = return l
getArrLoc l (a:ac) = do
    (VArr m) <- getLocValue l
    let (Acc e) = a
    (VInt v) <- semE e
    l2 <- if member v m 
            then return $ m ! v
            else throwError "Index out of bound!"
    l3 <- getArrLoc l2 ac
    return l3
