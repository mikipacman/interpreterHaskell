module ProgramTypes where
import AbsGramatyka
import Data.Map (Map, insert, (!), empty, fromList, size, adjust, member)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

-- Types

type MyMonad a = StateT Memory (ReaderT Env (ExceptT String IO)) a

data ValueUnion = VInt Integer 
                | VBool Bool 
                | VStr String 
                | VFun Fun 
                | VArr (Map Integer Location)
    deriving (Show)

newtype Fun = Fun ([ValueUnion] -> MyMonad ValueUnion)
instance Show Fun where
    show f = "function"

type Env = Map Ident Location

type Location = Integer
type VarMap = Map Location ValueUnion

type Cost = Integer
type Holder = Location

type ReadCost = Map Location (Cost, Holder) 
type WriteCost = Map Location (Cost, Holder) 
type OpCost = Map Op (Cost, Holder) 

data Memory = Memory 
    { vm :: VarMap
    , rc :: ReadCost
    , wc :: WriteCost
    , oc :: OpCost                          
    }
    deriving (Show)

-- Helper functions

io :: IO a -> MyMonad a
io = lift . lift . lift

newLoc :: MyMonad Location
newLoc = do
    s <- get
    let mem = vm s
    return (toInteger $ size mem)

getValue :: Ident -> MyMonad ValueUnion
getValue i = do
    l <- getLoc i
    v <- getLocValue l
    return v

getLoc :: Ident -> MyMonad Location
getLoc i = do
    env <- ask
    if member i env
    then return $ env ! i
    else throwError "Accessing undefined variable or function!"

getLocValue :: Location -> MyMonad ValueUnion
getLocValue l = do
    s <- get
    let mem = vm s
    return $ mem ! l

setNewValue :: Ident -> ValueUnion -> MyMonad Env
setNewValue i v = do
    s <- get
    let mem = vm s
    env <- ask
    l <- newLoc
    put ( s { vm = insert l v mem } )
    let new_env = insert i l env
    return new_env

setValue :: Ident -> ValueUnion -> MyMonad ()
setValue i v = do
    env <- ask
    let l = env ! i
    setLoc l v
    return ()

setLoc :: Location -> ValueUnion -> MyMonad ()
setLoc l v = do
    s <- get
    let mem = vm s
    put ( s { vm = adjust (const v) l mem } )
    return ()    

mapToDecl :: (FuncDeclItem, ValueUnion) -> Decl
mapToDecl (FDItem t i, VInt v) = VDecl t [Init i (ExprLit (IntL v))] 
mapToDecl (FDItem t i, VBool v) = case v of 
    True -> VDecl t [Init i (ExprLit TrueL)] 
    False -> VDecl t [Init i (ExprLit FalseL)] 


-- Built in functions 

get_init_memory_state :: Memory
get_init_memory_state = 
    Memory  { vm = fromList [(0, VFun $ Fun print_int_func)
                            ,(1, VFun $ Fun print_str_func)
                            ,(2, VFun $ Fun read_int_func)
                            ,(3, VFun $ Fun read_str_func)]
            ,rc = empty
            ,wc = empty
            ,oc = empty
            }

get_init_env :: Env
get_init_env = fromList [(Ident "printInt", 0)
    ,(Ident "printStr", 1)
    ,(Ident "readInt", 2)
    ,(Ident "readStr", 3)]

print_int_func :: [ValueUnion] -> MyMonad ValueUnion
print_int_func args = do
    if length args == 1
    then do
        let (VInt i) = args !! 0
        io $ print i
        return (VInt 0)
    else throwError $ "Wrong number of arguments in printInt call!"


print_str_func :: [ValueUnion] -> MyMonad ValueUnion
print_str_func args = do
    if length args == 1
    then do
        let (VStr i) = args !! 0
        io $ putStrLn i
        return (VInt 0)
    else throwError $ "Wrong number of arguments in printStr call!"


read_int_func :: [ValueUnion] -> MyMonad ValueUnion
read_int_func args = do
    if length args == 0
    then do
        line <- io $ getLine
        case (reads line) :: [(Integer, String)] of
            [(int, "")] -> return $ VInt (int :: Integer)
            otherwise   -> throwError $ "That is not a number!"
    else throwError $ "Wrong number of arguments in readInt call!"


read_str_func :: [ValueUnion] -> MyMonad ValueUnion
read_str_func args = do
    if length args == 0
    then do
        line <- io $ getLine
        return (VStr line)
    else throwError $ "Wrong number of arguments in readInt call!"
