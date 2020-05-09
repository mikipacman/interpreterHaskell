module ProgramTypes where
import AbsGramatyka
import Data.Map (Map, insert, (!), empty, fromList, size, adjust, member)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

-- Types

type DoubleMonad a = StateT MemoryState (ReaderT Env (ExceptT String IO)) a

data ValueUnion = VInt Integer 
                | VBool Bool 
                | VStr String 
                | VFun Fun 
                | VArr (Map Integer Location)
                | NoValue
    deriving (Show)

newtype Fun = Fun ([ValueUnion] -> DoubleMonad ValueUnion)
instance Show Fun where
    show f = "function"

type Location = Integer
type Env = Map Ident Location
type MemoryState = Map Location ValueUnion

type Cost = Integer
type Holder = Location

type ReadCost = Map Location (Cost, Holder) 
type WriteCost = Map Location (Cost, Holder) 
type OpCost = Map Location (Cost, Holder) 

-- Helper functions

io :: IO a -> DoubleMonad a
io = lift . lift . lift

newLoc :: DoubleMonad Location
newLoc = do
    mem <- get
    return (toInteger $ size mem)

getValue :: Ident -> DoubleMonad ValueUnion
getValue i = do
    l <- getLoc i
    v <- getLocValue l
    return v

getLoc :: Ident -> DoubleMonad Location
getLoc i = do
    env <- ask
    if member i env
    then return $ env ! i
    else throwError "Accessing undefined variable of function!"

getLocValue :: Location -> DoubleMonad ValueUnion
getLocValue l = do
    mem <- get
    return $ mem ! l

setNewValue :: Ident -> ValueUnion -> DoubleMonad Env
setNewValue i v = do
    mem <- get
    env <- ask
    l <- newLoc
    put $ insert l v mem
    let new_env = insert i l env
    return new_env

setValue :: Ident -> ValueUnion -> DoubleMonad ()
setValue i v = do
    env <- ask
    let l = env ! i
    setLoc l v
    return ()

setLoc :: Location -> ValueUnion -> DoubleMonad ()
setLoc l v = do
    mem <- get
    put $ adjust (const v) l mem
    return ()    

mapToDecl :: (FuncDeclItem, ValueUnion) -> Decl
mapToDecl (FDItem t i, VInt v) = VDecl t [Init i (ExprLit (IntL v))] 
mapToDecl (FDItem t i, VBool v) = case v of 
    True -> VDecl t [Init i (ExprLit TrueL)] 
    False -> VDecl t [Init i (ExprLit FalseL)] 



-- Built in functions 

get_init_memory_state :: MemoryState
get_init_memory_state = fromList [(0, VFun $ Fun print_int_func)
    ,(1, VFun $ Fun print_str_func)
    ,(2, VFun $ Fun read_int_func)
    ,(3, VFun $ Fun read_str_func)]

get_init_env :: Env
get_init_env = fromList [(Ident "printInt", 0)
    ,(Ident "printStr", 1)
    ,(Ident "readInt", 2)
    ,(Ident "readStr", 3)]

print_int_func :: [ValueUnion] -> DoubleMonad ValueUnion
print_int_func args = do
    if length args == 1
    then do
        let (VInt i) = args !! 0
        io $ print i
        return NoValue
    else throwError $ "Wrong number of arguments in printInt call!"


print_str_func :: [ValueUnion] -> DoubleMonad ValueUnion
print_str_func args = do
    if length args == 1
    then do
        let (VStr i) = args !! 0
        io $ putStrLn i
        return NoValue
    else throwError $ "Wrong number of arguments in printStr call!"


read_int_func :: [ValueUnion] -> DoubleMonad ValueUnion
read_int_func args = do
    if length args == 0
    then do
        line <- io $ getLine
        case (reads line) :: [(Integer, String)] of
            [(int, "")] -> return $ VInt (int :: Integer)
            otherwise   -> throwError $ "That is not a number!"
    else throwError $ "Wrong number of arguments in readInt call!"


read_str_func :: [ValueUnion] -> DoubleMonad ValueUnion
read_str_func args = do
    if length args == 0
    then do
        line <- io $ getLine
        return (VStr line)
    else throwError $ "Wrong number of arguments in readInt call!"
