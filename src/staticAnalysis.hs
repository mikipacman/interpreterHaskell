module StaticTyping where
import AbsGramatyka
import Data.Map (Map, insert, (!), empty, fromList, member, adjust)
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad (when)

type TypeEnv = Map Ident Type

data Type = BasicType BasicType | ArrayType AType | FuncType FuncType
    deriving (Eq, Ord, Show, Read)

data AType = AType BasicType Integer
    deriving (Ord, Show, Read)

instance Eq AType where
    (AType b1 i1) == (AType b2 i2) = (b1 == b2) && (i1 == i2)

data FuncType = FunT BasicType [BasicType]
    deriving (Eq, Ord, Show, Read)

type DoubleMonad a = ReaderT TypeEnv (ExceptT String IO) a


verifyProgram :: Int -> Program -> IO Bool
verifyProgram v p = do
    result <- runExceptT (runReaderT (checkProgram p) get_built_in_type_env)
    case result of
        (Right _) -> do
            when (v > 1) $ putStrLn "[Static type checking succeeded]\n"
            return True
        (Left e) -> do
            when (v > 1) $ putStrLn "[Static type checking failed]"
            putStrLn ("Error occured during types analysis: " ++ e)
            return False

get_built_in_type_env :: TypeEnv
get_built_in_type_env = fromList    [(Ident "printInt", FuncType $ FunT IntT [IntT])
                                    ,(Ident "printStr", FuncType $ FunT IntT [StringT])
                                    ,(Ident "readInt", FuncType $ FunT IntT [])
                                    ,(Ident "readStr", FuncType $ FunT StringT [])]

setType :: Ident -> Type -> DoubleMonad TypeEnv
setType i t = do
    typeEnv <- ask
    let newEnv = insert i t typeEnv
    return newEnv


getType :: Ident -> DoubleMonad Type
getType i = do
    typeEnv <- ask
    let (Ident name) = i
    if member i typeEnv
    then return $ typeEnv ! i
    else throwError $ (show name) ++ " is undefined in this scope"


assertTypeIdent :: Ident -> Type -> String -> DoubleMonad ()
assertTypeIdent i t s = do
    realType <- getType i
    if t == realType
    then return ()
    else throwError s

assertTypeExpr :: Expr -> Type -> String -> DoubleMonad ()
assertTypeExpr e t s = do
    realType <- checkExpr e 
    if t == realType
    then return ()
    else throwError s

assertTypeExprs :: [Expr] -> Type -> String -> DoubleMonad ()
assertTypeExprs [] _ _ = return ()
assertTypeExprs (e:es) t s = do
    assertTypeExpr e t s
    assertTypeExprs es t s

checkProgram :: Program -> DoubleMonad ()
checkProgram (Program []) = return ()
checkProgram (Program (d:ds)) = do
    newEnv <- checkDecl d
    local (const newEnv) $ checkProgram $ Program ds


checkExprs :: [Expr] -> DoubleMonad [Type]
checkExprs [] = return []
checkExprs (e:es) = do
    t <- checkExpr e
    ts <- checkExprs es
    return (t:ts)


checkExpr :: Expr -> DoubleMonad Type
checkExpr e = do
    case e of
        ExprLit lit -> case lit of
            IntL _      -> return $ BasicType IntT
            StringL _   -> return $ BasicType StringT
            TrueL       -> return $ BasicType BoolT
            FalseL      -> return $ BasicType BoolT
        ExprGC _ -> return $ BasicType IntT
        ExprBr expr -> do
            t <- checkExpr expr
            return t
        ExprCall i exprs -> do
            let (Ident name) = i
            t <- getType i
            ts <- checkExprs exprs
            case t of
                FuncType (FunT b params) -> do
                    if ts == (map (\p -> BasicType p) params)
                    then return $ BasicType b
                    else throwError $ "Param types do not match in " ++ (show name) ++ " call"
                _ -> throwError $ (show name) ++ " is not a function"
        ExprAcc i a -> do
            let (Ident name) = i
            t <- getType i
            case t of 
                ArrayType (AType b d) -> do
                    if d == toInteger (length a) 
                    then return $ BasicType b
                    else throwError $ "Only accessing single values of " ++ (show name) ++ " array is possible"
                BasicType b -> do
                    if length a == 0
                    then return $ BasicType b
                    else throwError $ (show name) ++ " is not an array"
                FuncType f -> throwError $ (show name) ++ " is a function"
        Neg e -> do
            t <- checkExpr e
            case t of
                BasicType IntT -> return $ BasicType IntT
                _ -> throwError "Minus sign works only with integers"
        Not e -> do
            t <- checkExpr e
            case t of
                BasicType BoolT -> return $ BasicType BoolT
                _ -> throwError "Negation sign works only with bools"
        EMul e1 op e2 -> do
            assertTypeExpr e1 (BasicType IntT) $ (show op) ++ " works only with integers"
            assertTypeExpr e2 (BasicType IntT) $ (show op) ++ " works only with integers"
            return (BasicType IntT)
        EAdd e1 op e2 -> do
            assertTypeExpr e1 (BasicType IntT) $ (show op) ++ " works only with integers"
            assertTypeExpr e2 (BasicType IntT) $ (show op) ++ " works only with integers"
            return (BasicType IntT)
        ERel e1 op e2 -> do
            assertTypeExpr e1 (BasicType IntT) $ (show op) ++ " works only with integers"
            assertTypeExpr e2 (BasicType IntT) $ (show op) ++ " works only with integers"
            return (BasicType BoolT)
        EEq e1 op e2 -> do
            t1 <- checkExpr e1
            t2 <- checkExpr e2
            case t1 of
                FuncType f1 -> throwError $ (show op) ++ " does not work with functions"
                other1 -> case t2 of
                    FuncType f2 -> throwError $ (show op) ++ " does not work with functions"
                    other2 -> if other1 == other2
                        then return $ BasicType BoolT
                        else throwError "types need to match when compared"
        EAnd e1 op e2 -> do
            assertTypeExpr e1 (BasicType BoolT) $ (show op) ++ " works only with bools"
            assertTypeExpr e2 (BasicType BoolT) $ (show op) ++ " works only with bools"
            return (BasicType BoolT)
        EOr e1 op e2 -> do
            assertTypeExpr e1 (BasicType BoolT) $ (show op) ++ " works only with bools"
            assertTypeExpr e2 (BasicType BoolT) $ (show op) ++ " works only with bools"
            return (BasicType BoolT)


checkDecls :: [Decl] -> DoubleMonad TypeEnv
checkDecls [] = ask
checkDecls (d:ds) = do
    newEnv <- checkDecl d 
    local (const newEnv) $ checkDecls ds


checkDecl :: Decl -> DoubleMonad TypeEnv
checkDecl d = case d of
    VDecl b [] ->  ask
    VDecl b (v:vs) -> do
        newEnv <- case v of
            NoInit i -> setType i $ BasicType b
            Init i e -> do
                t <- checkExpr e
                let (Ident name) = i
                if t == (BasicType b)
                then setType i $ BasicType b
                else throwError ("Type error in variable declaration " ++ show name)
        local (const newEnv) $ checkDecl $ VDecl b vs
    ADecl b [] -> ask
    ADecl (ArrT b a) (v:vs) -> do
        let declType = ArrayType (AType b $ toInteger (length a))
        newEnv <- case v of
            NoInit i -> setType i declType
            Init i e -> do
                t <- checkExpr e
                let (Ident name) = i
                if t == declType
                then setType i declType
                else throwError ("Type error in array declaration " ++ show name)
        local (const newEnv) $ checkDecl $ ADecl (ArrT b a) vs
    FDecl b i fdis ds stmts e -> do
        let (Ident name) = i
        let mapFDI (FDItem b i) = VDecl b [NoInit i]
        let mapTypes (FDItem b _) = b
        let funcType = FuncType (FunT b $ map mapTypes fdis)
        let param_decl = map mapFDI fdis
        
        env1 <- checkDecls param_decl
        env2 <- local (const env1) $ setType i funcType
        env3 <- local (const env2) $ checkDecls ds

        local (const env3) $ checkStmts stmts
        retType <- local (const env3) $ checkExpr e

        if retType == (BasicType b)
        then setType i funcType
        else throwError ("Type error in return value in function " ++ show name)


checkStmts :: [Stmt] -> DoubleMonad ()
checkStmts [] = return ()
checkStmts (s:ss) = do
    checkStmt s
    checkStmts ss


checkStmt :: Stmt -> DoubleMonad ()
checkStmt s = case s of
    EmptyStmt -> return ()
    IfStmt e (Block s) -> do
        assertTypeExpr e (BasicType BoolT) "Expression in if statement must be bool"
        checkStmts s   

    IfElseStmt e (Block s1) (Block s2) -> do
        assertTypeExpr e (BasicType BoolT) "Expression in if statement must be bool"
        checkStmts s1
        checkStmts s2
    WhileStmt e (Block s) -> do
        assertTypeExpr e (BasicType BoolT) "Expression in while statement must be bool"
        checkStmts s   
    ForStmt d es as (Block s) -> do
        case d of 
            FDecl _ _ _ _ _ _-> throwError "Cant define function inside for loop"
            _ -> do
                newEnv <- checkDecl d 
                local (const newEnv) $ assertTypeExprs es (BasicType BoolT) "All expressions in the middle of for loop have to be bool type"
                local (const newEnv) $ checkStmts $ map (\x -> AsStmt x) as
                local (const newEnv) $ checkStmts s
    AsStmt (VarAs i a e) -> do
        let (Ident name) = i
        t <- getType i
        case t of
            BasicType b -> do
                if length a == 0
                then assertTypeExpr e t $ "Type does not match in assigning " ++ (show name)
                else throwError $ (show name) ++ "is not an array"
            ArrayType (AType b d) -> do
                if d == (toInteger $ length a)
                then assertTypeExpr e (BasicType b) $ "Type does not match in assigning " ++ (show name)
                else throwError $ "Assigning parts of array " ++ (show name) ++ " is not possible"
            FuncType f -> throwError $ "Cannot assign to function " ++ (show name)
    ExprStmt e -> do
        checkExpr e 
        return ()
    SCostStmt c -> case c of
        CRead i1 e i2 -> do
            let (Ident name1) = i1
            let (Ident name2) = i2
            assertTypeIdent i2 (BasicType IntT) $ (show name2) ++ " in count statement should be integer"
            assertTypeExpr e (BasicType IntT) $ "Expression in count statement should be integer"
            t1 <- getType i1
            case t1 of
                FuncType f -> throwError $ (show name1) ++ " in count statement can't be function"
                _ -> return ()
        CWrite i1 e i2 -> do
            let (Ident name1) = i1
            let (Ident name2) = i2
            assertTypeIdent i2 (BasicType IntT) $ (show name2) ++ " in count statement should be integer"
            assertTypeExpr e (BasicType IntT) $ "Expression in count statement should be integer"
            t1 <- getType i1
            case t1 of
                FuncType f -> throwError $ (show name1) ++ " in count statement can't be function"
                _ -> return ()
        COp op e i -> do
            let (Ident name) = i
            assertTypeExpr e (BasicType IntT) $ (show name) ++ " in count statement should be integer"
            return ()
