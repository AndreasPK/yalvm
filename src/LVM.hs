{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}


module LVM where

import LuaLoader
import LuaObjects
import qualified Data.Map.Strict as Map
import Control.Monad.ST as ST
import Data.Either
import Data.Maybe
import Data.STRef
import Data.Array.IArray as Array
import qualified Data.Bits as Bits
import System.IO
import Data.IORef
import Debug.Trace as Trace
import qualified Parser
import qualified Control.Monad as Monad
import Control.Exception.Base
import Data.Function
import qualified Data.Traversable as Traversable
import qualified Data.Foldable as Foldable
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe
import qualified Data.ByteString as BS
import LRT

registerFunction :: LuaState -> LuaFunctionInstance -> IO ()
registerFunction state f =
  let name = funcName f in
    writeGlobal (stateGlobals state) (name ++ "\NUL") $ LOFunction f {funcName = name ++ "\NUL"}
  --setGlobal state name $ LOFunction $ HaskellFunctionInstance name f


registerAll :: LuaState -> IO ()
registerAll s = registerFunction s LRT.lrtPrint

type LuaGlobals = IORef (Map.Map String LuaObject)

-- | Fully describes the state of the virtual machine
data LuaState = LuaState {
  stateExecutionThread :: {-# UNPACK #-} !LuaExecutionThread,        --current execution Thread
  stateGlobals :: {-# UNPACK #-} !(IORef (Map.Map String LuaObject)) --Global list
  }
  deriving (Eq)

instance Show LuaState where
  show state = "(LuaState " ++ show (stateExecutionThread state) ++
    show (unsafePerformIO $ readIORef $ stateGlobals state) ++
      ")"

instance Show LuaGlobals where
  show x = "Globals: " ++ unsafePerformIO ( show <$> readIORef x)

--Take the function header of a top level function and create a vm state based on that
startExecution :: Parser.LuaFunctionHeader -> IO LuaState
startExecution header = do
  function@(LOFunction funcInstance) <- linstantiateFunction header :: IO LuaObject
  let exec = LuaExecutionThread funcInstance (error "Can't execute past top of call stack") 0 LuaStateRunning callInfoEmpty 1
  LuaState exec <$> newIORef Map.empty

runLuaCode :: FilePath -> IO LVM.LuaState
runLuaCode path = do
  --traceM "runLuaCode"
  fileContent <- BS.readFile path :: IO BS.ByteString
  --traceM "fr"
  let chunk = either (error "Failed to parse binary file") id $ LuaLoader.loadLua fileContent -- :: IO (Either String Parser.LuaBinaryFile)
  vm <- LVM.startExecution $ Parser.getTopFunction chunk :: IO LVM.LuaState
  --traceM "vc"
  registerAll vm
  --traceM "Run chunk"
  LVM.runLuaFunction vm
  --traceM "ranLuaCode"

lGetResults :: LuaState -> LVStack
lGetResults = unsafePerformIO . lGetStateStack --fromList . execResults . stateExecutionThread

--only valid while lua function is being executed
lGetStateStack :: LuaState -> IO LVStack
lGetStateStack state = do
  uv <- readIORef $ (funcUpvalues . execFunctionInstance . stateExecutionThread) state
  return $ getStack uv
  where
    getStack (FuncUpvalueStack stack _) = stack

-- | Get upvalue structure of the currently active function
lGetCurrentUpvalues :: LuaState -> LuaFunctionUpvalues
lGetCurrentUpvalues = funcUpvalues . execFunctionInstance . stateExecutionThread

lGetLastFrame :: LuaState -> LuaExecutionThread
lGetLastFrame = execPrevInst . stateExecutionThread

lGetRunningState :: LuaState -> LuaExecutionState
--lGetRunningState (LuaState (LuaExecutionThread _ _ pc execState callInfoEmpty) globals) = execState
--lGetRunningState _ = LuaStateSuspended
lGetRunningState = execRunningState . stateExecutionThread

lIsHaskellFunction :: LuaState -> Bool
lIsHaskellFunction (LuaState (LuaExecutionThread HaskellFunctionInstance {} _ _ _ _ _) globals) = True
lIsHaskellFunction _ = False

lSetExecutionState :: LuaState -> LuaExecutionState -> LuaState
lSetExecutionState (LuaState exec globals) state =
   LuaState exec { execRunningState = state } globals

lGetGlobals :: LuaState -> IORef (Map.Map String LuaObject)
lGetGlobals (LuaState _ globals) = globals


-- | Applies the given function to the Lua stack
updateStack :: LuaState -> (LVStack -> IO LVStack) -> IO LuaState
updateStack state f = do
  let exec = stateExecutionThread state
      func = execFunctionInstance exec
  uv <- readIORef $ funcUpvalues func
  let oldStack = uvStack uv :: LVStack
  modifyStack oldStack f
  --stack <-  f $! oldStack :: IO LVStack
  return state --Since stack is mutable we don't need to explicitly update the state
  --return $ state { stateExecutionThread = exec { execFunctionInstance = func { funcUpvalues = uv { uvStack = stack } }}}
--When only values on the stack are modified this is a faster alternative then to update the whole state 'stack'
modifyStack :: LVStack -> (LVStack -> IO LVStack) -> IO ()
modifyStack stack f = do
  f stack
  return ()


getInstruction :: LuaState -> LuaInstruction
getInstruction state  = --(LuaState (LuaExecutionThread (LuaFunctionInstance _ instructions _ _ _ _) _ pc _ _) _) =
  let !exec = stateExecutionThread state
  in
  (funcInstructions . execFunctionInstance) exec `UV.unsafeIndex`{-(!)-} execCurrentPC exec
  -- instructions !! pc

--get the instruction at pc + offset
getRelativeInstruction :: LuaState -> Int -> LuaInstruction
getRelativeInstruction state offset =
  let func = execFunctionInstance . stateExecutionThread $ state
  in
  (funcInstructions func UV.! (getPC state + offset))


-- | Program counter manipulation
setPC :: LuaState -> Int -> LuaState
setPC state newipc =
  state { stateExecutionThread =  (stateExecutionThread state) { execCurrentPC = newipc } }


getPC :: LuaState -> Int
getPC =
  execCurrentPC . stateExecutionThread

incPC :: LuaState -> LuaState
incPC s = setPC s $ 1 + getPC s

readGlobal :: IORef (Map.Map String LuaObject) -> String -> IO LuaObject
readGlobal g s = fromMaybe LONil . Map.lookup s <$> readIORef g

writeGlobal :: LuaGlobals -> String -> LuaObject -> IO ()
writeGlobal g k v = modifyIORef g (Map.insert k v)

-- | Upvalue manipulation

ra :: LuaInstruction -> Int
rb :: LuaInstruction -> Int
rc :: LuaInstruction -> Int
rbx :: LuaInstruction -> Int
rsbx :: LuaInstruction -> Int
ra = LuaLoader.ra
rb = LuaLoader.rb
rc = LuaLoader.rc
rbx = LuaLoader.rbx
rsbx = LuaLoader.rsbx

-- |Applies the changes from executing a OP code to the lua state
execOP :: LuaState -> IO LuaState
execOP state = do
  --Variables for easier writing of op code functions
  --state@(LuaState executionThread@(LuaExecutionThread functionInst prevInst pc execState callInfo) globals) <- state
  --let  LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues _ = functionInst
  --traceM $ show (instructions, pc)

  let exec = {-# SCC "getExec" #-} stateExecutionThread state
      func = {-# SCC "getFunc" #-} seq exec $ execFunctionInstance exec
      pc = execCurrentPC exec
      instructions = seq func $ funcInstructions func
      nextInstruction = UV.unsafeIndex instructions pc
      --nextInstruction = {-# SCC "decode" #-} getInstruction state --instructions V.! getPC state
      opCode = {-# SCC "decode" #-} seq nextInstruction $ LuaLoader.op nextInstruction :: LuaOPCode
      ra = {-# SCC "decode" #-} LuaLoader.ra $! nextInstruction :: Int
      rb = {-# SCC "decode" #-} LuaLoader.rb nextInstruction :: Int
      rc = {-# SCC "decode" #-} LuaLoader.rc nextInstruction
      rbx = {-# SCC "decode" #-} LuaLoader.rbx nextInstruction :: Int
      rsbx = {-# SCC "decode" #-} LuaLoader.rsbx nextInstruction :: Int
      uv = funcUpvalues func
      constList = {-# SCC "decode" #-} Parser.fhConstants . funcHeader $ func -- . execFunctionInstance . stateExecutionThread $ state
      globals = lGetGlobals state :: IORef (Map.Map String LuaObject)

  stack <- lGetStateStack state

       --For K enccoding bit 9 deternines if we use a constant or a stack value
  let  decodeConst :: Int -> IO LuaObject
       decodeConst register = if (register Bits..&. 256) == 256 then
         return (getConst constList (register - 256)) else
           getElement stack register

--  putStrLn "\n"
--  ss <- stackSize stack
--  putStr $ "StackSize:" ++ show ss ++ " - "
--  putStrLn $ show (getPC state + 1) ++ ":" ++ ppLuaInstruction nextInstruction
--  printStack state
--  putStrLn "\n\n"

  {-# SCC "switchOP" #-} case opCode of
    MOVE -> do -- RA = RB
      obj <- getElement stack rb
      modifyStack stack $ \s -> setElement s ra obj
      execOP $ incPC state
    LOADNIL -> -- S[RA..RB] = nil
      incPC <$> updateStack state (\s -> setRange s ra $ LONil:Prelude.replicate (rb - ra) LONil)
      --return $ incPC $ updateStack state (\(LuaMap m) -> LuaMap $ foldl (\m k -> Map.insert k LONil m) stackMap [ra..rb])
    LOADK -> do
      modifyStack stack (\stack -> setElement stack ra $ getConst constList rbx)
      execOP $! incPC state
    LOADBOOL ->
      let value = LOBool $ rb /= 0 :: LuaObject
      in
      fmap incPC $ flip updateStack (\stack -> setElement stack ra value) $ --set boolean value
        if rc /= 0 then incPC state else state --increment pc based on rc(or not)
    GETGLOBAL -> do -- R(A) := Glb(Kst(rbx))
      let key = loString $ getConst constList rbx :: String
      value <- readGlobal globals key
      --print key
      modifyStack stack (\stack -> setElement stack ra value)
      execOP $ incPC state
    SETGLOBAL -> do
      let s = loString $ getConst constList rbx :: String
      value <- getElement stack ra
      writeGlobal globals s value
      --print globals
      --traceM $ "set" ++  show (s, value)
      execOP $ incPC state
    GETUPVAL -> do
      let uv = lGetCurrentUpvalues state
      value <- seq uv $ readUpvalue uv rb
      seq value $ setElement stack ra value
      return $ incPC state
    SETUPVAL -> do
      let uv = lGetCurrentUpvalues state
      value <- getElement stack ra
      writeUpvalue uv rb value
      return $ incPC state
      --error "Set upvalue not yet supported"
    GETTABLE -> do
      index <- decodeConst rc :: IO LuaObject
      LOTable table <- getElement stack rb :: IO LuaObject
      table <- readIORef table
      let value = getTableElement table index
      --traceShow(table, value, index) $
      modifyStack stack $ \s -> setElement s ra value
      return $ incPC state
    SETTABLE -> do
      value <- decodeConst rc :: IO LuaObject
      LOTable table <- getElement stack ra
      index <- decodeConst rb :: IO LuaObject
      modifyIORef' table (\t -> setTableElement t index value)
      --let newTable = LOTable $ setTableElement table index value
      --traceShow (ra, table, newTable)
      return $ incPC state -- updateStack state $ \s -> setElement s ra newTable
    ADD -> {-# SCC "addCost" #-} do
      x <- ltoNumber <$> decodeConst rb
      y <- seq x $ ltoNumber <$> decodeConst rc
      seq y $ setElement stack ra $! ladd x y
      execOP $ incPC state
    SUB -> do
      x <- ltoNumber <$> decodeConst rb
      y <- ltoNumber <$> decodeConst rc
      incPC <$> updateStack state (\s -> setElement s ra $ lsub x y)
    MUL -> do
      x <- ltoNumber <$> decodeConst rb
      y <- ltoNumber <$> decodeConst rc
      incPC <$> updateStack state (\s -> setElement s ra $ lmul x y)
    DIV -> do
      x <- ltoNumber <$> decodeConst rb
      y <- ltoNumber <$> decodeConst rc
      incPC <$> updateStack state (\s -> setElement s ra $ ldiv x y)
    MOD -> do
      x <- ltoNumber <$> decodeConst rb
      y <- ltoNumber <$> decodeConst rc
      incPC <$> updateStack state (\s -> setElement s ra $ lmod x y)
    POW -> do
      x <- ltoNumber <$> decodeConst rb
      y <- ltoNumber <$> decodeConst rc
      incPC <$> updateStack state (\s -> setElement s ra $ lpow x y)
    UNM -> do
      LONumber x <- ltoNumber <$> getElement stack rb
      incPC <$> updateStack state (\s -> setElement s ra (LONumber (-x)))
    NOT -> do
      LOBool value <- ltoBool <$> getElement stack rb --Convert RB to boolean value
      let raVal = if value then LOBool False else LOBool True --invert value
      incPC <$> updateStack state (\stack -> setElement stack ra raVal)
    LEN -> do
      obj <- getElement stack rb
      incPC <$> updateStack state (\stack -> setElement stack ra $ lLen obj)
    CONCAT -> do
      res <- concatOP stack rb rc
      incPC <$> updateStack state (\stack -> setElement stack ra res)
    JMP ->
      return $ incPC $ setPC state $ rsbx + getPC state
    CLOSURE ->
      incPC <$> execCLOSURE state ra rbx
    VARARG -> do
      let callInfo = execCallInfo . stateExecutionThread $ state
      state <- updateStack state (\s -> updateVarArg s callInfo ra rb)
      return $ incPC state
    SELF -> do
      LOTable table <- getElement stack rb :: IO LuaObject --Reference to the table
      index <- decodeConst rc
      callee <- do t <- readIORef table; return $ getTableElement t index :: IO LuaObject
      let setCallee = (\s -> setElement s ra callee) :: LVStack -> IO LVStack
      let setTable = (\s -> setElement s (ra+1) $ LOTable table) :: LVStack -> IO LVStack
      let update s = Monad.join $ setCallee <$> setTable s
      incPC <$> updateStack state update
    LuaLoader.EQ -> do
      a <- decodeConst rb
      b <- decodeConst rc
      let comparison = a == b
          result     = not $ compBtoI comparison ra
      return $ incPC $ if result then incPC state
          else state
    LuaLoader.LT -> do
      a <- decodeConst rb
      b <- decodeConst rc
      let comparison = a < b
          result     = not $ compBtoI comparison ra
      return $ incPC $ if result then incPC state
          else state
    LuaLoader.LE -> do
      a <- decodeConst rb
      b <- decodeConst rc
      let comparison = a <= b
          result     = not $ compBtoI comparison ra
      return $ incPC $ if result then incPC state
          else state
    TEST -> do
      LOBool a <- ltoBool <$> getElement stack ra :: IO LuaObject
      let match = compBtoI a rc
      return $ incPC $ if not match then incPC state
        else state
    TESTSET -> do
      LOBool b <- ltoBool <$> getElement stack rb :: IO LuaObject
      let match = compBtoI b rc
      incPC <$> if match then updateStack state (\stack -> setElement stack ra =<< getElement stack rb)
        else return $ incPC state
    FORPREP -> do
      step <- getElement stack $ ra+2
      index <- Monad.liftM2 lsub (getElement stack ra) $ return step
      let newPC = getPC state + rsbx
      modifyStack stack $ \stack -> setElement stack ra index
      execOP $ setPC state $ newPC + 1
    FORLOOP -> {-# SCC "forCost" #-} do
      {- R(A) += R(A+2)
      if R(A) <?= R(A+1) then {
      PC += sBx; R(A+3) = R(A)
      } -}

      stepping <- getElement stack $! seq ra $ ra + 2
      index <- ladd stepping <$> getElement stack ra
      limit <- getElement stack $! ra + 1

      let comparison = if lvNumber stepping >= 0 then (<=) else (>=) :: Double -> Double -> Bool --Check depends on sign of stepping
          check = Data.Function.on comparison lvNumber -- (\a b -> comparison (lvNumber a) (lvNumber b))

      setElement stack ra index --update index variable

      let withinLimit = check index limit --False when out of bounds
      execOP =<< {-# SCC "forCost2" #-} incPC <$> if withinLimit
        then do
          setElement stack (ra+3) index
          return $ setPC state $ rsbx + getPC state
        else return state

    TFORLOOP ->
      error "TFORLOOP Undefined"
    NEWTABLE -> do
      tref <- newIORef createTable
      incPC <$> updateStack state (\stack -> setElement stack ra $ LOTable tref)
    SETLIST -> do
      -- used to set an array of elements at once
      LOTable table <- getElement stack ra
      oldTable <- readIORef table
      elementCount <- if rb > 0 then return rb else do s <- stackSize stack; return $ s - (ra + 1)
      elements <- getRange stack (ra+1) (ra+elementCount) :: IO [LuaObject]
      let index = if rc /= 0 then (rc - 1) * 50 + 1 else error "large indices not supported"
      let newPairs = zip (map (LONumber . fromIntegral) [index ..]) elements
      let newTable = foldl (\t (k, v) -> setTableElement t k v) oldTable newPairs
      writeIORef table newTable
      return $ incPC state
    CALL -> runCall state
    TAILCALL -> tailCall state
    RETURN -> returnCall state
    _ -> error "Unknown OP-Code"


compBtoI :: Bool -> Int -> Bool
compBtoI b i = b && i > 0 || not b && i == 0

--copies values from vararg list to the stack, might update top of stack
updateVarArg :: (LuaStack a) => a -> LuaCallInfo -> Int -> Int -> IO a
updateVarArg stack (LuaCallInfo varargs) offset countIndicator
  | countIndicator == 0 = do
      stackSize <- stackSize stack --maximum size of stack
      let argCount = length varargs
      let requiredSize = offset + argCount - 1
      stack <- if stackSize <= requiredSize then return stack else setStackSize stack $ requiredSize - stackSize
      Monad.zipWithM_ (setElement stack) [offset..] varargs
      return stack
      --foldl (\s (k, v) -> setElement s k v) stack $ zip [offset .. ] varargs
  | countIndicator > 1 = do Monad.zipWithM_ (setElement stack) [offset..] varargs; return stack --foldl (\s (k, v) -> setElement s k v) stack $ zip [offset .. (offset + countIndicator - 2)] varargs

gatherUpvalue :: LuaState -> LuaInstruction -> IO UVRef
gatherUpvalue state instruction = do
  stack <- lGetStateStack state :: IO LVStack
  upvalues <- readIORef uvref :: IO FuncUpvalueList
  seq upvalues $ case opcode of
    MOVE -> return $ UVRef stack rb
    GETUPVAL -> return $ getUpvalue upvalues rb
  where
    (opcode, _, rb, _, _, _) = decodeLuaInstruction instruction
    uvref = funcUpvalues . execFunctionInstance . stateExecutionThread $ state :: LuaFunctionUpvalues



--create a new closure, ra -> closure storeage, rbx -> function index
execCLOSURE :: LuaState -> Int -> Int -> IO LuaState
execCLOSURE state@(LuaState executionThread globals) ra rbx = do
  -- instantiate closure without upvalues
  let funcPrototype = getContainedFunctionHeader executionThread rbx :: Parser.LuaFunctionHeader
  LOFunction func <- linstantiateFunction funcPrototype :: IO LuaObject

  -- create upvalue references. Pointer in haskell are not fun ...
  let upvalueCount = Parser.fhUpvalueCount funcPrototype
      upvalueInstructions = seq upvalueCount $ fmap (getRelativeInstruction state) [1 .. fromIntegral upvalueCount] :: [LuaInstruction]
      closureUpvalueRef = funcUpvalues func :: LuaFunctionUpvalues

  closureUpvalueList <- mapM (gatherUpvalue state) upvalueInstructions :: IO [UVRef]
  setUpvalues closureUpvalueRef closureUpvalueList

  let pc = getPC state
  let func1 = func --{ funcUpvalues = undefined }
      --TODO: create upvalues
  let closure = LOFunction func1
  (`setPC` (pc + fromIntegral upvalueCount)) <$> updateStack state (\stack -> setElement stack ra closure)


--Continue stepping the VM until we reach a return statement
runLuaFunction :: LuaState -> IO LuaState
runLuaFunction state =
  --In case we reached end of execution return resulting state
  if isRunning state
    then runLuaFunction =<< execOP state
    else return state
    where
      isRunning = (LuaStateRunning ==) . lGetRunningState :: LuaState -> Bool

--if the current instruction is a call instruction return the function to be called
getCallee :: LuaState -> IO LuaObject
getCallee state = do
  let inst = getInstruction state
      ra = LVM.ra inst
  stack <- lGetStateStack state
  getElement stack ra

isLuaCall :: LuaState -> IO Bool
isLuaCall state = do
  (LOFunction x) <- getCallee state
  return $ ft x
    where
      ft :: LuaFunctionInstance -> Bool
      ft LuaFunctionInstance {} = True
      ft HaskellFunctionInstance {} = False


-- |  We enter the function with the pc being at the instruction after the call
  --    and the execution frame still being that of the caller.
  --
  --  When calling a function we perform the following operations:
  --    Collecting parameters of the target function:
  --    a) Collect fixed parameters, pass these onto the stack of the callee
  --    b) Collect variable parameters for vararg calls and put them into callInfo
  --
  --    c) Stack cleanup: We remove all collected parameters from the callers Stack as well as the function object
  --
  --    d) Jumping back into runLuaFunction with the called function being active
  --
runCall :: LuaState -> IO LuaState
runCall state = do
  islc <- isLuaCall state
  if islc then runLuaCall state else runHaskellCall state

-- | returns the called function and its parameters, ra is callee function
--   rb encodes the number of parameters
--   rc encodes the number of return values (not used)
getCallArguments :: LuaState -> IO (LuaObject, [LuaObject])
getCallArguments state = do
  let callInst = getInstruction state -- Call instruction
      calleePosition = LVM.ra callInst
  stack <- lGetStateStack state
  parameters <- collectValues (calleePosition +1) (LVM.rb callInst) stack
  callee <- getCallee state
  return (callee, parameters)

-- |Remove the functionInstance and arguments from the stack
--  Then run the haskell function passing the arguments. Afterwards
--  take the requested amount of results from the Haskell function and place
--  them on the stack of the caller
runHaskellCall :: LuaState -> IO LuaState
runHaskellCall state = do
  --traceM "runHaskellCall"
  --Get function arguments
  (callee, parameters) <- getCallArguments state :: IO (LuaObject, [LuaObject])
  let func = funcFunc . loFunction $ callee :: LVStack -> IO LVStack
  --traceM $ "runHaskellCall:" ++ show (loFunction callee, parameters)
  --Call haskell function with arguments, transform results back to State
  results <- func =<< (fromList parameters :: IO LVStack)


  --Trim results according to rc
  rlist <- toList results :: IO [LuaObject]
  let inst = getInstruction state
  let requestCount = LVM.rc inst
  results <- return $ if requestCount == 0 then rlist else take (requestCount-1) rlist


  let stackUpdate s = setRange s (LVM.ra inst) results
  rres <- updateStack state stackUpdate
  return $ incPC rres


runLuaCall :: LuaState -> IO LuaState
runLuaCall state = do
  (calledFunction, parameters) <- getCallArguments state
  let callee = loFunction calledFunction
  let calleeHeader = funcHeader callee

  let fixedArgCount = fromIntegral $ Parser.fhParameterCount calleeHeader

  let (fixedArguments, varArgs) = splitAt fixedArgCount parameters :: ([LuaObject], [LuaObject])

  --Pad arguments with Nil of neccesary in order to pass at least 'fixedArgCount' many arguments
  fixedArguments <- return $ take fixedArgCount $ fixedArguments ++ repeat LONil :: IO [LuaObject]


  calleeStack <- createStack $ fromIntegral $ Parser.fhMaxStacksize calleeHeader :: IO LVStack
  calleeStack <- setRange calleeStack 0 fixedArguments

  let upvalues = funcUpvalues callee :: LuaFunctionUpvalues
  setUVStack upvalues calleeStack

--  callee <- return $ callee { funcUpvalues = upvalues }

  --traceM "calleeStack"
  --mapM_ print =<< toList calleeStack

  let oldExec = stateExecutionThread state
  let newExecutionThread = LuaExecutionThread callee oldExec 0 LuaStateRunning (LuaCallInfo varArgs) 0
  return $ LuaState newExecutionThread $ lGetGlobals state



-- Collects the number of parameters given by rb in the call instruction
-- offset gives the position of the first parameter, rb the expected parameter count
collectValues :: (LuaStack a, Show a) => Int -> Int -> a -> IO [LuaObject]
collectValues offset rbCount stack = do
--  ss <- stackSize stack
--  mapM_ (\i -> print =<< getElement stack i) [0.. ss - 1]

  let rc = compare rbCount 1
  result <- case rc of
    Prelude.EQ -> return [] --  rbCount == 1 = return []
    Prelude.GT -> getRange stack offset (offset + rbCount -2)
    Prelude.LT -> do ss <- stackSize stack; getRange stack offset (ss - 1)

  return ()
  --traceM $ "collectValues: " ++ show (offset,rbCount, result)
  return result



-- | Called with the current state when encountering a return instruction
{-
When returning from a function we need to:

a) Collect the values we want to return
b) Push the returned values on the callers stack
c) Reset the execution context back to the one of the caller
d) TODO: CLose upvalues
-}

returnCall :: LuaState -> IO LuaState
returnCall state = do
  let returnInstruction = getInstruction state
  --print $ ppLuaInstruction returnInstruction
  --collect results
  stack <- lGetStateStack state :: IO LVStack
  results <- collectValues (LVM.ra returnInstruction) (LVM.rb returnInstruction) stack :: IO [LuaObject]
  --traceM $ "returnCall - Results:" ++ show results

  --b - c are handled by this function

  returnByOrigin state results


returnByOrigin :: LuaState -> [LuaObject] -> IO LuaState
--When returning to Haskell we only pass back the list of results
returnByOrigin state results =
  --traceShow ("returnByOrigin" ++ show state) $
  if (execStop . stateExecutionThread) state == 1 then do
    r <- fromList results :: IO LVStack
    updateStack state $ const $ return r
    return $ setStateDead state
    else
      returnFromLua state results
  --return $ LuaState (LuaExecInstanceTop results) globals
  where
    setStateDead :: LuaState -> LuaState
    setStateDead state =
      let exec = stateExecutionThread state in state { stateExecutionThread = exec { execRunningState = LuaStateDead } }

--Returning back to a caller lua function
returnFromLua :: LuaState -> [LuaObject] -> IO LuaState
returnFromLua state calleeResults = do
  --The instruction
  --traceIO "returnByOrigin: Returning to Lua Caller"
  --(LuaState (LuaExecutionThread _ prevExecInst _ _ callInfo) globals) <- state

  let returnInstruction = getInstruction state
  --print $ ppLuaInstruction returnInstruction
  --Number of results to return based on the previous call code, 0 variable, 1 = none, > 1 = n - 1
  let resultCount = LVM.rb . getInstruction $ state
  --traceIO $ show resultCount ++ "=RB"
  let results = if resultCount == 0 then calleeResults else take (resultCount - 1) $ calleeResults ++ repeat LONil
  let stackOffset = LVM.ra returnInstruction
  --traceM $ show calleeResults

  --Update the previous stack frame, remove parameters not requrested
  let prevFrame = lGetLastFrame state
      globals = stateGlobals state
  let state = LuaState { stateExecutionThread = prevFrame, stateGlobals = globals }


  let callInstruction = getInstruction state
  let callResultCount = LVM.rc callInstruction
  let resultPlacement = LVM.ra callInstruction
  --traceM $ "callInstruction: " ++ ppLuaInstruction callInstruction
  let callResults = if callResultCount == 0 then results else take (callResultCount - 1) $ results ++ repeat LONil

    --Prevent blowing up the stack for unlimited return values
  callerStack <- lGetStateStack state
  ss <- stackSize callerStack
  state <- if callResultCount == 0 && length callResults + resultPlacement > ss
              then updateStack state (\s -> setStackSize s (length callResults + resultPlacement))
                else return state


  newState <- updateStack state $ \s -> setRange s resultPlacement callResults --pushObjects returnedResults
  s <- toList <$> lGetStateStack newState
  --traceM $ "New stack:" ++ show s

  return $ incPC newState


getCalledFunction :: LuaObject -> LuaFunctionInstance
getCalledFunction (LOFunction f) = f
getCalledFunction _ = error "Tried to called a object which is not a function!"

-- pc points to the tailcall function
-- replace current execution Thread by one execution the callee
tailCall :: LuaState -> IO LuaState
tailCall state = do
  (callee, parameters) <- getCallArguments state :: IO (LuaObject, [LuaObject])
  let calledFunction = getCalledFunction callee

  --If we have a tailcall to a non lua function something REALLY went wrong, so guarantee this with an assert
  x <- isLuaCall state :: IO Bool
  return $ assert x ()

  let newStackSize = lgetMaxStackSize callee :: Int

  --collect  parameters
  let maxArgCount = lgetArgCount callee :: Int
  let (fixedArgs, varArgs) = splitAt maxArgCount parameters
  newStack <- createStack $ lgetMaxStackSize callee
  newStack <- setRange newStack 0 fixedArgs
  --newStack <- fromList fixedArgs :: IO LVStack --Monad.join $ flip setStackSize newStackSize $ flip pushObjects fixedArgs $ createStack 0 :: IO LuaMap

  let isTop = execStop . stateExecutionThread $ state
  let prevExecInst = if 1 == isTop then error "Can't execute past top of callstack" else lGetLastFrame state
  let globals = lGetGlobals state
  let newState = LuaState (LuaExecutionThread calledFunction prevExecInst 0 LuaStateRunning (LuaCallInfo varArgs) isTop) globals

  --updateStack fails here?!?
  updateStack newState $ return . const newStack


concatOP :: (LuaStack a) => a -> Int -> Int -> IO LuaObject
concatOP stack from to =
  case compare from to of
    Prelude.GT -> return $ LOString ""
    _ {-otherwise-} -> do
      x <- concat <$> mapM
        (\i -> do e <- getElement stack i; return (init . loString $ ltoString e))
        [from..to]

      return $ LOString x -- $ traceShowId $ start ++ next


printStack :: LuaState -> IO ()
printStack state = do
  print "printStack:"
  do
  --Monad.unless ((execStop . stateExecutionThread) state == 1) $ do
      stack <- lGetStateStack state
      ss <- stackSize stack
      Foldable.traverse_ (\n -> do e <- getElement stack n; putStrLn $ show n ++ ":" ++ show e) [0..ss - 1]

{-
stackWalk :: LVM.LuaState -> IO ()
stackWalk state = do
  print "Stackwalk"
  ps state
  return ()
  where
    ps (LVM.LuaState (LuaObjects.LuaExecInstanceTop res) _) =
      mapM_ print res
    ps state = do
      let stack = lGetStateStack state
      let prevInst = execPrevInst $ stateExecutionThread state
      ss <- stackSize stack
      Foldable.traverse_ (fmap print . getElement stack) [0..ss - 1]
      print "1-UP"
      ps $ LVM.LuaState prevInst Map.empty
-}
