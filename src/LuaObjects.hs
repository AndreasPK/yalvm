module LuaObjects(module LuaObjects) where

import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Mutable as V
import Control.Monad.ST as ST
--import Control.Monad.ST.Lazy
import Data.Primitive.Array
import Data.Maybe

import LuaLoader
import Parser as P


data LuaObject = LONil | LONumber { lvNumber :: !Double} | LOString String | LOTable LTable |
  LOFunction LuaFunctionInstance | LOBool Bool | LOUserData {-TODO-} | LOThread {-TODO-}
  deriving (Eq, Show, Ord)

getConst :: LuaConstList -> Int -> LuaObject
getConst (LuaConstList size constants) pos =
  let constant = constants !! fromIntegral pos
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
  let attributes = show (op inst) : map show [ ra inst :: Int, rb inst, rc inst, rsbx inst, rbx inst] :: [String]
      namedAtt = flip zip attributes $ fmap (++":") ["op", "ra", "rb", "rc", "rsbx", "rbx"] :: [(String, String)]
  in
  foldl (\a b -> a ++ " " ++ uncurry (++) b) "" namedAtt

isNil :: LuaObject -> Bool
isNil o
  | o == LONil = True
  | otherwise = False

ltoNumber :: LuaObject -> LuaObject
ltoNumber n@(LONumber _) = n
ltoNumber (LOString s) = LONumber $ read s
ltoNumber x = error $ "Can't convert " ++ show x ++ " to number"

ltoBool :: LuaObject -> LuaObject
ltoBool LONil = LOBool True
ltoBool (LOBool False) = LOBool False
ltoBool _ = LOBool True

lLen :: LuaObject -> LuaObject
lLen (LOString s) = LONumber $ fromIntegral $ length s
lLen _ = LONumber undefined --in lua implemented via metamethods

ltoString :: LuaObject -> LuaObject
ltoString LONil = LOString "nil"
ltoString (LOBool True) = LOString "true"
ltoString (LOBool False) = LOString "false"
ltoString (LONumber x) = LOString $ show x
ltoString x@(LOString s) = x
ltostring x = LOString $ show x

lgetFunctionHeader :: LuaObject -> LuaFunctionHeader
lgetFunctionHeader (LOFunction (LuaFunctionInstance stack instructions constList functionHeader varargs upvalues)) =
  functionHeader

lgetPrototype :: LuaObject -> Int -> LuaFunctionHeader
lgetPrototype (LOFunction (LuaFunctionInstance stack instructions constList functionHeader varargs upvalues)) pos =
  let LuaFunctionHeader name startLine endLine upvalueCount parameterCount varargFlag stackSize instructions constList functionList instPosList localList upvalueList = functionHeader
  in
  functionList !! pos

-- | Create a function instance based on prototype, doesn't set up passed parameters/upvalues/varargs
linstantiateFunction :: LuaFunctionHeader -> LuaObject
linstantiateFunction functionHeader@(LuaFunctionHeader name startLine endLine upvalueCount
  parameterCount varargFlag stackSize instructions constList functionList
  instPosList localList upvalueList) =
  let
  functionInstance =
    LuaFunctionInstance
      (createStack $ fromIntegral stackSize )
      ( (\(LuaInstructionList l ins) -> ins) instructions)
      constList
      functionHeader
      (LuaMap (Map.empty :: Map.Map Int LuaObject))
      (LuaRTUpvalueList IntMap.empty)
  in
  LOFunction functionInstance


--Set the upvalues for a function instance
lsetupvalues :: LuaObject -> LuaRTUpvalueList -> LuaObject
lsetupvalues (LOFunction (LuaFunctionInstance stack inst constList fh varargs _)) upvalues =
  LOFunction (LuaFunctionInstance stack inst constList fh varargs upvalues)

-- | Set the variable argument list based on the given list of values
-- lsetvarargs function arguments -> updatedFunction
lsetvarargs :: LuaObject -> [LuaObject] -> LuaObject
lsetvarargs (LOFunction (LuaFunctionInstance stack inst constList fh _ upvalues)) parameters =
  let varargs = LuaMap $ Map.fromList $ zip [0..] parameters
  in
  LOFunction (LuaFunctionInstance stack inst constList fh varargs upvalues)

lgetArgCount :: LuaObject -> Int
lgetArgCount (LOFunction (LuaFunctionInstance _ _ _ fh _ _)) =
  let (LuaFunctionHeader _ _ _ upvalCount parCount varargFlags _ _ _ _ _ _ _) = fh
  in fromIntegral parCount

lisVarArg :: LuaObject -> Bool
lisVarArg (LOFunction (LuaFunctionInstance _ _ _ fh _ _)) =
  let (LuaFunctionHeader _ _ _ upvalCount parCount varargFlags _ _ _ _ _ _ _) = fh
  in varargFlags /= 0

lgetMaxStackSize :: LuaObject -> Int
lgetMaxStackSize (LOFunction (LuaFunctionInstance _ _ _ fh _ _)) =
  let (LuaFunctionHeader _ _ _ upvalCount parCount varargFlags stackSize _ _ _ _ _ _) = fh
  in fromIntegral stackSize


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
  createStack :: Int -> l
  setElement :: l -> Int -> LuaObject -> l
  getElement :: l -> Int -> LuaObject
  getRange :: l -> Int -> Int -> [LuaObject]--get elements stack[a..b]
  setRange :: l -> Int -> [LuaObject] -> l
  stackSize :: l -> Int
  setStackSize :: l -> Int -> l
  pushObjects :: l -> [LuaObject] -> l
  fromList :: [LuaObject] -> l
  fromList = pushObjects (createStack 0)
  shrink :: l -> Int -> l --shrink stack by x elements if possible
  shrink s x = setStackSize s $ stackSize s - x
  toList :: l -> [LuaObject]
  toList l = map (getElement l) [0..stackSize l -1]
  setRange stack n objects = foldl (\s (k, v) -> setElement s k v) stack $ zip [n..] objects --requires stack to be at least (n - 1) in size

-- | Maps indexes to a lua object
newtype LuaMap = LuaMap (Map.Map Int LuaObject) deriving (Eq, Show, Ord)

instance LuaStack LuaMap where
  setElement (LuaMap stack) pos obj = LuaMap $ Map.insert pos obj stack
  getElement (LuaMap stack) pos = fromMaybe LONil $ Map.lookup pos stack
  createStack size = LuaMap $ Map.fromAscList $ zip [0..size-1] $ repeat LONil
  stackSize (LuaMap stack) = Map.size stack
  getRange stack lb rb = map (getElement stack :: Int -> LuaObject) [lb .. rb]
  setStackSize (LuaMap stack) size
    | Map.size stack == size = LuaMap stack
    | Map.size stack > size  = LuaMap $ fst $ Map.split size stack
    | Map.size stack < size = LuaMap $ Map.union stack $ Map.fromAscList $ zip [Map.size stack .. (size-1)] $ repeat LONil
  pushObjects (LuaMap stack) objects = LuaMap $ Map.union stack $
    Map.fromAscList $ zip [(Map.size stack)..] objects

-- | the runtime value of a upvalue
data LuaRuntimUpvalue =
  LRTUpvalueReference LuaMap Int | --Reference a stack and a index
  LRTUpvalueValue LuaObject | --Reference a object directly
  LRTUpvalueUpvalue LuaRTUpvalueList Int | --References the upvalue of another function
  LRTOpenUpvalue Int Bool --position type(Stackvalue=True/Upvalue)
  deriving (Eq, Show, Ord)

-- | List of upvalues used in this function
data LuaRTUpvalueList = LuaRTUpvalueList
  (IntMap.IntMap LuaRuntimUpvalue)
  deriving (Eq, Show, Ord)

dereferenceUpvalue :: LuaRuntimUpvalue -> LuaObject
dereferenceUpvalue (LRTUpvalueValue x) = x
dereferenceUpvalue (LRTUpvalueReference m i) = getElement m i
dereferenceUpvalue (LRTUpvalueUpvalue (LuaRTUpvalueList upvalList) pos) = dereferenceUpvalue $ upvalList IntMap.! pos

updateUpvalue :: LuaObject -> LuaRuntimUpvalue -> LuaRuntimUpvalue
updateUpvalue obj (LRTUpvalueReference m i)  = LRTUpvalueReference (setElement m i obj) i
updateUpvalue obj _  = LRTUpvalueValue obj

getUpvalue :: LuaRTUpvalueList -> Int -> LuaObject
getUpvalue (LuaRTUpvalueList m) i = fromMaybe LONil (dereferenceUpvalue <$> IntMap.lookup i m :: Maybe LuaObject)

setUpvalue :: LuaRTUpvalueList -> Int -> LuaObject -> LuaRTUpvalueList
setUpvalue (LuaRTUpvalueList m) i obj =
  let oldValue = fromJust $ IntMap.lookup i m :: LuaRuntimUpvalue
  in
  LuaRTUpvalueList $ IntMap.adjust (updateUpvalue obj) i m

type LuaParameterList = [LuaObject]


-- | Instance of an executable function
-- LuaFunctionInstance stack instructions constants funcPrototypes upvalues arguments
data LuaFunctionInstance =
  LuaFunctionInstance
  { funcStack :: !LuaMap -- Stack
  , funcInstructions :: ![LuaInstruction] --List of op codes
  , funcConstants :: !LuaConstList --List of constants
  , funcHeader :: !LuaFunctionHeader --Function prototypes
  , funcVarargs :: !LuaMap --ArgumentList for varargs, starting with index 0
  , funcUpvalues :: !LuaRTUpvalueList --Upvalues missing
  }
  |
  HaskellFunctionInstance
  { funcName :: !String --name
  , funcStack :: !LuaMap --Stack
  , funcFunc ::  IO LuaMap -> IO LuaMap
  }
  deriving ()

instance Eq LuaFunctionInstance where
   (==) a b = error "Broken typeclass for function instances"

instance Ord LuaFunctionInstance where
  (<=) a b = error "Broken typeclass for function instances"

instance Show LuaFunctionInstance where
  show (HaskellFunctionInstance name _ _) = "(HaskellFunction: " ++ name ++ ")"
  show (LuaFunctionInstance stack _ constList fh varargs upvalues) = "(Lua Function: " ++ show (stack, constList, fh, varargs, upvalues) ++ ")"




data LuaExecutionState = LuaStateSuspended | LuaStateRunning | LuaStateDead deriving (Eq, Show, Ord, Enum)

-- | LuaExecutionThread currentInstance prevInstance position state
-- Wraps a function AND execution frame, so jumping into another function also means replacing the current executin thread as well
-- LuaExecutionThread func precThread pc state callInfo
data LuaExecutionThread =
  LuaExecutionThread {
    execFunctionInstance :: !LuaFunctionInstance,
    execPrevInst :: !LuaExecutionThread,
    execCurrentPC :: !Int,
    execRunningState :: !LuaExecutionState,
    execCallInfo :: !LuaCallInfo }
  |
  LuaExecInstanceTop [LuaObject]
  deriving (Eq, Show, Ord)

-- | Contains information about arguments passed to the function
data LuaCallInfo = LuaCallInfo
  [LuaObject]
  deriving(Show, Eq, Ord)

callInfo :: [LuaObject] -> LuaCallInfo
callInfo = LuaCallInfo

callInfoEmpty = LuaCallInfo []

getContainedFunctionHeader :: LuaExecutionThread -> Int -> LuaFunctionHeader
getContainedFunctionHeader (LuaExecutionThread (LuaFunctionInstance _ _ _ functionHeader _ _ ) _ _ _ _)  =
  getIndexedFunction functionHeader
