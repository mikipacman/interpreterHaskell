module ProgramTypes where
import AbsGramatyka
import Data.Map (Map, insert, (!), empty, fromList, size, adjust)
import Control.Monad.State
import Control.Monad.Reader

-- Types

type DoubleMonad a = StateT MemoryState (ReaderT Env IO) a

data ValueUnion = VInt Integer | VBool Bool | VFun Fun | NoValue
    deriving (Show)

newtype Fun = Fun ([ValueUnion] -> DoubleMonad ValueUnion)
instance Show Fun where
    show f = "function"

type Location = Integer
type Env = Map Ident Location
type MemoryState = Map Location ValueUnion

-- Helper functions

io :: IO a -> DoubleMonad a
io = lift . lift

reader :: ReaderT Env IO a -> DoubleMonad a
reader = lift

newLoc :: DoubleMonad Location
newLoc = do
    mem <- get
    return (toInteger $ size mem)

getValue :: Ident -> DoubleMonad ValueUnion
getValue i = do
    mem <- get
    env <- ask
    return (mem ! (env ! i))

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
    mem <- get
    env <- ask
    let l = env ! i
    put $ adjust (const v) l mem
    return ()


mapToDecl :: (FuncDeclItem, ValueUnion) -> Decl
mapToDecl (FDItem t i, VInt v) = VDecl t [Init i (ExprLit (IntL v))] 
mapToDecl (FDItem t i, VBool v) = case v of 
    True -> VDecl t [Init i (ExprLit TrueL)] 
    False -> VDecl t [Init i (ExprLit FalseL)] 


-- Built in functions 

get_init_memory_state :: MemoryState
get_init_memory_state = fromList [(0, VFun $ Fun print_func), (1, VFun $ Fun read_int_func)]

get_init_env :: Env
get_init_env = fromList [(Ident "print", 0), (Ident "readInt", 1)]

print_func :: [ValueUnion] -> DoubleMonad ValueUnion
print_func [] = return NoValue
print_func ((VInt i):ps) = do       -- TODO add error when #args is wrong
    io $ print i
    return NoValue

read_int_func :: [ValueUnion] -> DoubleMonad ValueUnion
read_int_func [] = do
    line <- io $ getLine
    let int = (read line)::Integer -- TODO add error when can't parse
    return $ VInt int

-- TODO add read string when string will be a legal type