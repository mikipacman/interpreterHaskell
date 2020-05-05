module ProgramTypes where
import AbsGramatyka
import Data.Map (Map, insert, (!), empty, fromList, size)
import Control.Monad.State

-- Types

type DoubleMonad a = StateT MemoryState IO a

data ValueUnion = VInt Integer | VBool Bool | VFun Fun | NoValue
    deriving (Show)

newtype Fun = Fun ([ValueUnion] -> DoubleMonad ValueUnion)
instance Show Fun where
    show f = "function"

type Location = Integer

data MemoryState = MemoryState 
    { id_map :: Map Ident Location
    , loc_map :: Map Location ValueUnion
    }   
    deriving (Show)


-- Helper functions

io :: IO a -> DoubleMonad a
io = lift

newLoc :: DoubleMonad Location
newLoc = do
    ms <- get
    let m = id_map ms
    return (toInteger $ size m)

getValue :: Ident -> DoubleMonad ValueUnion
getValue i = do
    state <- get
    let id_m = id_map state
    let loc_m = loc_map state
    return (loc_m ! (id_m ! i))

setNewValue :: Ident -> ValueUnion -> DoubleMonad ()
setNewValue i v = do
    state <- get
    l <- newLoc
    let id_m = id_map state
    let loc_m = loc_map state
    put (state 
        { id_map = insert i l id_m
        , loc_map = insert l v loc_m 
        })
    return ()

setValue :: Ident -> ValueUnion -> DoubleMonad ()
setValue i v = do
    state <- get
    let id_m = id_map state
    let loc_m = loc_map state
    let l = id_m ! i
    put (state 
        { loc_map = insert l v loc_m 
        })
    return ()


mapToDecl :: (FuncDeclItem, ValueUnion) -> Decl
mapToDecl (FDItem t i, VInt v) = VDecl t [Init i (ExprLit (IntL v))] 
mapToDecl (FDItem t i, VBool v) = case v of 
    True -> VDecl t [Init i (ExprLit TrueL)] 
    False -> VDecl t [Init i (ExprLit FalseL)] 


-- Built in functions 

get_init_memory_state :: MemoryState
get_init_memory_state = MemoryState 
    { id_map = fromList [(Ident "print", 0), (Ident "readInt", 1)]
    , loc_map =  fromList [(0, VFun $ Fun print_func), (1, VFun $ Fun read_int_func)]
    }

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