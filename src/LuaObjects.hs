{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}


module LuaObjects(module LuaObjects) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Foldable as F
import Control.Monad.ST as ST
--import Control.Monad.ST.Lazy
import Data.Primitive.Array
import Data.Maybe
import Data.IORef
import System.IO.Unsafe
import qualified Data.Sequence as Sequence
import Control.Monad as Monad

import LuaLoader
import Parser as P
import Debug.Trace
import Control.Exception.Base

data LuaObject = LONil | LONumber { lvNumber :: {-# UNPACK #-} !Double} | LOString { loString :: !String} | LOTable {-# UNPACK #-} !(IORef LTable) |
  LOFunction { loFunction :: !LuaFunctionInstance} | LOBool !Bool | LOUserData {-TODO-} | LOThread {-TODO-}
  deriving (Eq, Show, Ord)

type LuaRef = IORef LuaObject

instance Show (IORef LuaObject) where
  show o = show (unsafePerformIO $ readLR o)

instance Ord (IORef LuaObject) where
  (<=) a b = error "Order not defined for LuaRef"

instance Show (IORef LTable) where
  show r = "Some LuaTable"

instance Ord (IORef LTable) where
  (<=) = error "Can't compare table references"


readLR :: LuaRef -> IO LuaObject
readLR = readIORef

updateLR :: LuaRef -> (LuaObject -> LuaObject) -> IO ()
updateLR = modifyIORef'

writeLR :: LuaRef -> LuaObject -> IO ()
writeLR = writeIORef


getConst :: LuaConstList -> Int -> LuaObject
getConst (LuaConstList size constants) pos =
  let constant = constants V.! pos
  in
    luaConstToObject constant

luaConstToObject :: LuaConst -> LuaObject
luaConstToObject LuaNil = LONil
luaConstToObject (LuaConstNumber value) = LONumber value
luaConstToObject (LuaConstBool bool) = LOBool bool
luaConstToObject (LuaConstString (LuaString l s)) = LOString s

-- | Associates each Key with a Lua Object.
newtype LTable = LTable (Map.Map LuaObject LuaObject)  deriving (Eq, Show, Ord)


-- | getTableElement table key -> value
getTableElement :: LTable -> LuaObject -> LuaObject
getTableElement (LTable m) k
  | k == LONil = LONil
  | Map.member k m = m Map.! k
  | otherwise = LONil

-- | setTableElement table key value -> changed table
-- If the key is nil ignore the operation
-- If the value is nil remove the element from the map
-- Otherwise update/add the entry
setTableElement :: LTable -> LuaObject -> LuaObject -> LTable
setTableElement table@(LTable m) k v
  | isNil k = table
  | isNil v = LTable $ Map.delete k m
  | otherwise = LTable $ Map.insert k v m

createTable = LTable Map.empty :: LTable

ppLuaInstruction :: LuaInstruction -> String
ppLuaInstruction inst =
  let attributes = show (op inst) : map show [ ra inst :: Int, rb inst, rc inst, rsbx inst, rsbx inst + 131070, rbx inst] :: [String]
      namedAtt = flip zip attributes $ fmap (++":") ["op", "ra", "rb", "rc", "rsbx", "csbx" , "rbx"] :: [(String, String)]
  in
  foldl (\a b -> a ++ " " ++ uncurry (++) b) "" namedAtt

isNil :: LuaObject -> Bool
isNil o
  | o == LONil = True
  | otherwise = False

ltoNumber :: LuaObject -> LuaObject
ltoNumber n@(LONumber _) = n
ltoNumber (LOString s) = LONumber $ read s
ltoNumber x = error $ unsafePerformIO $ do
  print "ltoNumber: Can't convert to number"
  print x
  return "toNumber conversion error"

ltoBool :: LuaObject -> LuaObject
ltoBool LONil = LOBool True
ltoBool (LOBool False) = LOBool False
ltoBool _ = LOBool True

lLen :: LuaObject -> LuaObject
lLen (LOString s) = LONumber $ fromIntegral $ length s
lLen _ = error "Length of objects other than String not implemented"

ltoString :: LuaObject -> LuaObject
ltoString LONil = LOString "nil"
ltoString (LOBool True) = LOString "true"
ltoString (LOBool False) = LOString "false"
ltoString (LONumber x) = LOString $ show x
ltoString x@(LOString s) = x
ltoString x = error $ unsafePerformIO $ do
  putStrLn "Error:Can't convert type to string"
  print x
  return "Conversion error"


lgetFunctionHeader :: LuaObject -> LuaFunctionHeader
lgetFunctionHeader = funcHeader . loFunction

--functionInstance -> FunctionIndex -> FunctionHeader
lgetPrototype :: LuaObject -> Int -> LuaFunctionHeader
lgetPrototype =
  --let LuaFunctionHeader name startLine endLine upvalueCount parameterCount varargFlag stackSize instructions constList functionList instPosList localList upvalueList = functionHeader
  --in
  (!!) . P.fhFunctions . funcHeader . loFunction
  --functionList !! pos

-- | Create a function instance based on prototype, doesn't set up passed parameters/upvalues/varargs
linstantiateFunction :: LuaFunctionHeader -> IO LuaObject
linstantiateFunction functionHeader@(LuaFunctionHeader name startLine endLine upvalueCount
  parameterCount varargFlag stackSize instructions constList functionList
  instPosList localList upvalueList) = do
    stack <- createStack $ fromIntegral stackSize :: IO LVStack
    upvalues <- newIORef $ FuncUpvalueStack stack []
    let functionInstance = LuaFunctionInstance
          upvalues
          (lilInstructions instructions)
          constList
          functionHeader
          V.empty
    return $ LOFunction functionInstance


-- | Set the variable argument list based on the given list of values
-- lsetvarargs function arguments -> updatedFunction
lsetvarargs :: LuaObject -> [LuaObject] -> LuaObject
lsetvarargs (LOFunction func) parameters =
  let varargs = V.fromList parameters
  in
  LOFunction (func {funcVarargs = varargs}) -- upvalues closure)

lgetArgCount :: LuaObject -> Int
lgetArgCount (LOFunction func) =
  let (LuaFunctionHeader _ _ _ upvalCount parCount varargFlags _ _ _ _ _ _ _) = funcHeader func
  in fromIntegral parCount

--varargFlag != 0 represents a variable argument function
lisVarArg :: LuaObject -> Bool
lisVarArg (LOFunction func) =
  0 /= (P.fhVarargFlag . funcHeader) func

lgetMaxStackSize :: LuaObject -> Int
lgetMaxStackSize =
  fromIntegral . fhMaxStacksize . funcHeader . loFunction

--lgetMaxStackSize (LOFunction (LuaFunctionInstance _ _ _ fh _ _)) =
--  let (LuaFunctionHeader _ _ _ upvalCount parCount varargFlags stackSize _ _ _ _ _ _) = fh
--  in fromIntegral stackSize



ladd :: LuaObject -> LuaObject -> LuaObject
ladd (LONumber x) (LONumber y) = LONumber $ x + y

-- | sub a b = a - b
lsub :: LuaObject -> LuaObject -> LuaObject
lsub (LONumber x) (LONumber y) = LONumber $ x - y

lmul :: LuaObject -> LuaObject -> LuaObject
lmul (LONumber x) (LONumber y) = LONumber $ x * y

ldiv :: LuaObject -> LuaObject -> LuaObject
ldiv (LONumber x) (LONumber y) = LONumber $ x / y

-- | a % b == a - math.floor(a/b)*b
lmod :: LuaObject -> LuaObject -> LuaObject
lmod (LONumber a) (LONumber b) = LONumber $ a - b * (fromIntegral $ floor (a/b) :: Double)

lpow :: LuaObject -> LuaObject -> LuaObject
lpow (LONumber x) (LONumber y) = LONumber $ (**) x y




-- | Lua Stack wrapper TODO: Performance optimization
class LuaStack l where
  createStack :: Int -> IO l
  setElement :: l -> Int -> LuaObject -> IO l
  getElement :: l -> Int -> IO LuaObject
  getRange :: l -> Int -> Int -> IO [LuaObject]--get elements stack[a..b]
  setRange :: l -> Int -> [LuaObject] -> IO l --place given objects in stack starting at position p
  stackSize :: l -> IO Int
  setStackSize :: l -> Int -> IO l
  pushObjects :: l -> [LuaObject] -> IO l
  fromList :: [LuaObject] -> IO l
  fromList objs = do s <- createStack 0; pushObjects s objs
  shrink :: l -> Int -> IO l --shrink stack by x elements if possible
  shrink s x = do currentSize <- stackSize s; setStackSize s (currentSize - x)
  toList :: l -> IO [LuaObject]
  toList l = do count <- stackSize l; mapM (getElement l) [0..count -1]
  setRange stack n objects = foldM (\s (k, v) -> setElement s k v) stack $ zip [n..] objects --requires stack to be at least (n - 1) in size

type LVStack = IORef (MV.IOVector LuaObject)

showLVStack :: LVStack -> IO ()
showLVStack stack = do
  size <- stackSize stack
  let f = getElement stack
  mapM_ (f >=> print) [0..size - 1]

instance Show LVStack where
  show x = "LVStack (Show Not implemented)" --unsafePerformIO $ do ss <- stackSize x; show <$> mapM  (getElement x) [0 .. ss] --"LVStack [not implemented]"

instance LuaStack LVStack where
  createStack size = newIORef =<< MV.replicate size LONil
  setElement stack i v = do
    s <- readIORef stack :: IO (MV.IOVector LuaObject)
    MV.unsafeWrite s i v
    return stack
  getElement s i = readIORef s >>= flip MV.unsafeRead i
  getRange s from to = do stack <- readIORef s; mapM (MV.read stack) [from..to]
  setRange s start objects = do stack <- readIORef s; zipWithM_ (MV.write stack) [start..] objects; return s
  stackSize s = MV.length <$> readIORef s :: IO Int
  setStackSize sref size = do
    stack <- readIORef sref;
    case compare (MV.length stack) size of
      Prelude.EQ -> return sref
      Prelude.LT -> do
        let ss = MV.length stack
        writeIORef sref =<< MV.grow stack (size - ss)
        return sref;
      Prelude.GT -> do
        writeIORef sref $ MV.slice 0 (size-1) stack
        return sref
  --grows stack accordingly
  pushObjects sref objects = do
    --grow the vector by the number of elements, then put them into the vector
    let l = length objects
    --s <- stack :: IO LVStack
    stack <- readIORef sref
    let ss = MV.length stack

    writeIORef sref =<< MV.grow stack l
    setRange sref ss objects
  fromList objects = do
    stack <- createStack $ length objects
    setRange stack 0 objects
  shrink sref by = do
    stack <- readIORef sref
    let newSize = MV.length stack - by
    writeIORef sref $ MV.slice 0 newSize stack
    return sref
  toList sref = do
    stack <- readIORef sref
    mapM (MV.read stack) [0.. MV.length stack -1]

instance Show (IO LVStack) where
  show s = unsafePerformIO $ do s <- s; return $ show s


type LuaParameterList = [LuaObject]


data FuncUpvalueList =
  FuncUpvalueStack
  { uvStack :: LVStack
  , uvUpvalues :: [UVRef]
  }
  deriving (Eq, Show)-- Stack, list of indixed

data UVRef =
  UVRef
  { uvrStack :: LVStack
  , uvrOffset :: !Int
  }
  deriving (Eq, Show)

type LuaFunctionUpvalues = IORef FuncUpvalueList

instance Show LuaFunctionUpvalues where
  show x = unsafePerformIO $ do
    uv <- readIORef x
    return $ "(IORef (" ++ show uv ++ ") )"

setUVStack :: LuaFunctionUpvalues -> LVStack -> IO ()
setUVStack uvr stack = do
  uv <- readIORef uvr :: IO FuncUpvalueList
  writeIORef uvr $ uv { uvStack = stack }

getUpvalue :: FuncUpvalueList -> Int -> UVRef
getUpvalue (FuncUpvalueStack stack upvalues) index =
  upvalues !! index

readUpvalue :: LuaFunctionUpvalues -> Int -> IO LuaObject
readUpvalue funcUpval index = do
  uv <- (`getUpvalue` index) <$> readIORef funcUpval :: IO UVRef
  getElement (uvrStack uv) (uvrOffset uv) :: IO LuaObject


setUpvalues :: Traversable t => LuaFunctionUpvalues -> t UVRef -> IO LuaFunctionUpvalues
setUpvalues uvref upvalues = do
  modifyIORef' uvref $ \x -> x { uvUpvalues = F.toList upvalues }
  return uvref

-- | Update the value of the upvalue at the specific index
writeUpvalue :: LuaFunctionUpvalues -> Int -> LuaObject -> IO LuaFunctionUpvalues
writeUpvalue uvref index value = do
  upvalues <- readIORef uvref
  let uv = getUpvalue upvalues index
  setElement (uvrStack uv :: LVStack) (uvrOffset uv :: Int) (value :: LuaObject)
  return uvref





-- | Instance of an executable function
-- LuaFunctionInstance stack instructions constants funcPrototypes upvalues arguments
data LuaFunctionInstance =
  LuaFunctionInstance
  { funcUpvalues :: LuaFunctionUpvalues
  , funcInstructions :: !(UV.Vector LuaInstruction) --List of op codes
  , funcConstants :: !LuaConstList --List of constants
  , funcHeader :: !LuaFunctionHeader --Function prototypes
  , funcVarargs :: !(V.Vector LuaObject) --ArgumentList for varargs, starting with index 0
  }
  |
  HaskellFunctionInstance
  { funcName :: !String --name
  , funcFunc ::  LVStack -> IO LVStack
  }
  deriving ()

instance Eq LuaFunctionInstance where
   (==) a b = error "Broken typeclass for function instances"

instance Ord LuaFunctionInstance where
  (<=) a b = error "Broken typeclass for function instances"

instance Show LuaFunctionInstance where
  show (HaskellFunctionInstance name _) = "(HaskellFunction: " ++ name ++ ")"
  show (LuaFunctionInstance upvalues _ constList fh varargs ) = "(Lua Function: " ++ show ( uvStack (unsafePerformIO $ readIORef upvalues) , constList, fh, varargs, "upvalues not shown") ++ ")"

-- | Get line at which instruction was defined
-- function pc -> line
getLine :: LuaFunctionInstance -> Int -> Int
getLine =  (!!) . fhInstPos . funcHeader

data LuaExecutionState = LuaStateSuspended | LuaStateRunning | LuaStateDead deriving (Eq, Show, Ord, Enum)

-- | LuaExecutionThread currentInstance prevInstance position state
-- Wraps a function AND execution frame, so jumping into another function also means replacing the current executin thread as well
-- LuaExecutionThread func precThread pc state callInfo
data LuaExecutionThread =
  LuaExecutionThread {
    execFunctionInstance :: !LuaFunctionInstance,
    execPrevInst :: LuaExecutionThread,
    execCurrentPC :: !Int,
    execRunningState :: !LuaExecutionState,
    execCallInfo :: !LuaCallInfo,
    execStop :: {-# UNPACK #-} !Int } -- | 1: Stop VM, 0: Continue running
  deriving (Eq, Ord)

instance Show LuaExecutionThread where
  show x = if execStop x == 0 then "(LuaExecutionThread (" ++ show (execFunctionInstance x, execCurrentPC x, execRunningState x, execCallInfo x, execStop x) ++ "))"
    else "(LuaExecutionThread_Top " ++ (show . execFunctionInstance) x ++ ")"

-- | Contains information about arguments passed to the function
data LuaCallInfo = LuaCallInfo
  { lciParams :: ![LuaObject] }
  deriving(Show, Eq, Ord)

callInfo :: [LuaObject] -> LuaCallInfo
callInfo = LuaCallInfo

callInfoEmpty = LuaCallInfo []

getContainedFunctionHeader :: LuaExecutionThread -> Int -> LuaFunctionHeader
getContainedFunctionHeader  =
  getIndexedFunction . funcHeader . execFunctionInstance
