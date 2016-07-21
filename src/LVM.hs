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


type LuaGlobals = Map.Map String LuaObject

-- | Fully describes the state of the virtual machine
data LuaState = LuaState {
  stateExecutionThread :: !LuaExecutionThread,        --current execution Thread
  stateGlobals :: !(Map.Map String LuaObject) --Global list
  }
  deriving (Eq, Show, Ord)

--Take the function header of a top level function and create a vm state based on that
startExecution :: Parser.LuaFunctionHeader -> IO LuaState
startExecution header =
  let function@(LOFunction funcInstance) = linstantiateFunction header :: LuaObject
  in
  return $ LuaState (LuaExecutionThread funcInstance (LuaExecInstanceTop []) 0 LuaStateRunning callInfoEmpty) Map.empty

lGetResults :: (LuaStack results) => LuaState -> IO results
lGetResults = fromList . execResults . stateExecutionThread

--only valid while lua function is being executed
lGetStateStack :: LuaState -> IO LuaMap
lGetStateStack =
  funcStack . execFunctionInstance . stateExecutionThread

lGetLastFrame :: LuaState -> LuaExecutionThread
lGetLastFrame = execPrevInst . stateExecutionThread

lGetRunningState :: LuaState -> LuaExecutionState
lGetRunningState (LuaState (LuaExecutionThread _ _ pc execState callInfoEmpty) globals) = execState
lGetRunningState _ = LuaStateSuspended

lIsHaskellFunction :: LuaState -> Bool
lIsHaskellFunction (LuaState (LuaExecutionThread HaskellFunctionInstance {} _ _ _ _) globals) = True
lIsHaskellFunction _ = False

lSetExecutionState :: LuaState -> LuaExecutionState -> LuaState
lSetExecutionState (LuaState (LuaExecutionThread a b pc execState callInfoEmpty) globals) state =
   LuaState (LuaExecutionThread a b pc state callInfoEmpty) globals

lGetGlobals :: LuaState -> Map.Map String LuaObject
lGetGlobals (LuaState _ globals) = globals


-- | Applies the given function to the Lua stack
updateStack :: LuaState -> (LuaMap -> IO LuaMap) -> IO LuaState
updateStack state f =
  let exec = stateExecutionThread state
      func = execFunctionInstance exec
      stack = Monad.join $ f <$> funcStack func :: IO LuaMap
  in
  return $ state { stateExecutionThread = exec { execFunctionInstance = func { funcStack = stack }}}



getInstruction :: LuaState -> LuaInstruction
getInstruction state  = --(LuaState (LuaExecutionThread (LuaFunctionInstance _ instructions _ _ _ _) _ pc _ _) _) =
  let exec = stateExecutionThread state
      pos = execCurrentPC exec
  in
  (funcInstructions . execFunctionInstance) exec UV.! pos
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
execOP :: IO LuaState -> IO LuaState
execOP state = do
  --Variables for easier writing of op code functions
  state@(LuaState executionThread@(LuaExecutionThread functionInst prevInst pc execState callInfo) globals) <- state
  let  LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues _ = functionInst
       nextInstruction = instructions UV.! pc
       opCode = LuaLoader.op nextInstruction :: LuaOPCode
       ra = LuaLoader.ra nextInstruction :: Int
       rb = LuaLoader.rb nextInstruction
       rc = LuaLoader.rc nextInstruction
       rbx = LuaLoader.rbx nextInstruction
       rsbx = LuaLoader.rsbx nextInstruction

  stack <- stack --Extract the stack out of the io monad

       --For K enccoding bit 9 deternines if we use a constant or a stack value
  let  decodeConst :: Int -> IO LuaObject
       decodeConst register = if (register Bits..&. 256) == 256 then
         return (getConst constList (register - 256)) else
           getElement stack register

  let instruction = getInstruction state
      opCode = LuaLoader.op instruction
  --putStrLn $ show (getPC state + 1) ++ ":" ++ ppLuaInstruction instruction

  case opCode of
    MOVE -> do -- RA = RB
      obj <- getElement stack rb
      incPC <$> updateStack state (\s -> setElement s ra obj)
      --incPC <$> updateStack state (\stack ->setElement stack ra $ getElement stack rb)
    LOADNIL -> -- S[RA..RB] = nil
      incPC <$> updateStack state (\s -> setRange s ra $ LONil:Prelude.replicate (rb - ra) LONil)
      --return $ incPC $ updateStack state (\(LuaMap m) -> LuaMap $ foldl (\m k -> Map.insert k LONil m) stackMap [ra..rb])
    LOADK ->
      incPC <$> updateStack state (\stack -> setElement stack ra $ getConst constList rbx)
    LOADBOOL ->
      let value = LOBool $ rb /= 0 :: LuaObject
      in
      fmap incPC $ flip updateStack (\stack -> setElement stack ra value) $ --set boolean value
        if rc /= 0 then incPC state else state --increment pc based on rc(or not)
    GETGLOBAL -> -- R(A) := Glb(Kst(rbx))
      let LOString s = getConst constList rbx :: LuaObject
          value = fromMaybe (error $ "Global doesn't exist!!!" ++ show s ++ show globals) $ Map.lookup s globals
      in
      incPC <$> updateStack state (\stack -> setElement stack ra value)
    SETGLOBAL -> do
      let LOString s = getConst constList rbx :: LuaObject
      value <- getElement stack ra
      return $ incPC $ LuaState executionThread $ Map.insert s value globals
    GETUPVAL ->
      let value = error "Get upvalue" -- getUpvalue upvalues rb :: LuaObject
      in
      incPC <$> updateStack state (\s -> setElement s ra value)
    SETUPVAL ->
      --let value = getElement stack ra
      --    newUpvalues = error "Set upvalue" -- setUpvalue upvalues rb value
      --in
      error "Set upvalue"
    GETTABLE -> do
      index <- decodeConst rc :: IO LuaObject
      LOTable table <- getElement stack rb :: IO LuaObject
      table <- readIORef table
      let value = getTableElement table index
      traceShow(table, value, index) $
        fmap incPC $ updateStack state $ \s -> setElement s ra value
    SETTABLE -> do
      value <- decodeConst rc :: IO LuaObject
      LOTable table <- getElement stack ra
      index <- decodeConst rb :: IO LuaObject
      modifyIORef' table (\t -> setTableElement t index value)
      --let newTable = LOTable $ setTableElement table index value
      --traceShow (ra, table, newTable)
      return $ incPC state -- updateStack state $ \s -> setElement s ra newTable
    ADD -> do
      x <- ltoNumber <$> decodeConst rb
      y <- ltoNumber <$> decodeConst rc
      incPC <$> updateStack state (\s -> setElement s ra $ ladd x y)
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
    VARARG ->
      incPC <$> updateStack state (\s -> updateVarArg s callInfo ra rb)
    SELF -> do
      LOTable table <- getElement stack rb :: IO LuaObject --Reference to the table
      index <- decodeConst rc
      callee <- do t <- readIORef table; return $ getTableElement t index :: IO LuaObject
      let setCallee = (\s -> setElement s ra callee) :: LuaMap -> IO LuaMap
      let setTable = (\s -> setElement s (ra+1) $ LOTable table) :: LuaMap -> IO LuaMap
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
      incPC <$> if match then updateStack state (\stack -> Monad.join $ Monad.liftM (setElement stack ra) $ getElement stack rb)
        else return $ incPC state
    FORPREP -> do
      step <- getElement stack $ ra+2
      index <- Monad.liftM2 lsub (getElement stack ra) $ return step
      let newPC = getPC state + rsbx
      incPC <$> Monad.liftM2 setPC
        (updateStack state $ const $ setElement stack ra index) --Update ra in the stack
        (return newPC)
    FORLOOP -> do
      index <- Monad.liftM2 ladd (getElement stack $ ra + 2) (getElement stack ra)
      limit <- getElement stack $ ra + 1
      stepping <- getElement stack $ ra + 2
      let comparison = if lvNumber stepping >= 0 then (<=) else (>=) :: Double -> Double -> Bool --Check depends on sign of stepping
          check = Data.Function.on comparison lvNumber -- (\a b -> comparison (lvNumber a) (lvNumber b))
      newState <- updateStack state $ \s -> setElement s ra index
      incPC <$> if check index limit then
        Monad.liftM2 setPC
          (updateStack newState $ \s -> setElement s (ra+3) index )
          (return $ rsbx + getPC state)
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
    CALL -> runCall $ return state
    TAILCALL -> tailCall $ return state
    RETURN -> returnCall $ return state

    otherwise -> error "Unknown OP-Code"


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

--create a new closure
execCLOSURE :: LuaState -> Int -> Int -> IO LuaState
execCLOSURE state@(LuaState executionThread globals) ra rbx =
  let funcPrototype@(Parser.LuaFunctionHeader _ _ _ upvalueCount paramCount varargFlag stackSize instList constList _ _ _ _) = getContainedFunctionHeader executionThread rbx :: Parser.LuaFunctionHeader
      LOFunction func = linstantiateFunction funcPrototype :: LuaObject
      pc = getPC state
      upvalueInstructions = fmap (getRelativeInstruction state) [1 .. fromIntegral upvalueCount] :: [LuaInstruction]
      func1 = func { funcUpvalues = LuaRTUpvalueList undefined}

      --TODO: create upvalues
  in
  (`setPC` (pc + fromIntegral upvalueCount)) <$> updateStack state (\stack -> setElement stack ra (LOFunction func1))

--Continue stepping the VM until we reach a return statement
runLuaFunction :: IO LuaState -> IO LuaState
runLuaFunction state = do
  state <- state
  --In case we reached end of execution return resulting state
  if isRunning state
    then runLuaFunction $ execOP $ return state
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
runCall :: IO LuaState -> IO LuaState
runCall state = do
  s <- state
  islc <- isLuaCall s
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

runHaskellCall :: IO LuaState -> IO LuaState
runHaskellCall state = do
  us <- state
  (callee, parameters) <- getCallArguments us :: IO (LuaObject, [LuaObject])
  let LOFunction (HaskellFunctionInstance name _s func) = callee

  --Call haskell function with arguments, transform results back to State
  results <- func (fromList parameters :: IO LuaMap)

  --remove parameters + function from caller stack
  let stackUpdate s = shrink s $ length parameters
  state <- (`updateStack` stackUpdate) <$> state

  rlist <- toList results :: IO [LuaObject]

  returnFromHaskell state rlist

returnFromHaskell :: IO LuaState -> [LuaObject] -> IO LuaState
returnFromHaskell state results = do
  inst <- getInstruction <$> state
  let requestCount = LVM.rc inst
  results <- return $ if requestCount == 0 then results else take (requestCount-1) results

  let stackUpdate s = setRange s (LVM.ra inst) results
  s <- state
  rres <- updateStack s stackUpdate
  return $ incPC rres

runLuaCall :: IO LuaState -> IO LuaState
runLuaCall state = do
  state <- state
  let (LuaState executionThread@(LuaExecutionThread functionInst prevInst pc execState callInfo) globals) = state
  let (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues _ ) = functionInst

  (calledFunction, parameters) <- getCallArguments state
  let calleeInstance = (\(LOFunction f) -> f) calledFunction :: LuaFunctionInstance
  let calleeHeader = lgetFunctionHeader calledFunction :: Parser.LuaFunctionHeader
  --collect arguments
  let maxArgCount = lgetArgCount calledFunction :: Int

  --clearing caller stack
  let argCount = length parameters :: Int

  --remove objects used as parameters from the callers stack (shrink stack by count +1)
  nos@(LuaState oldExecutionThread _) <- updateStack state (\s -> do ss <- stackSize s; setStackSize s $ ss - (max argCount maxArgCount + 1))

  --split arguments in fixed args and varargs
  let (fixedArguments, varArgs) = splitAt maxArgCount parameters :: ([LuaObject], [LuaObject])

  calleeStack <- createStack maxArgCount :: IO LuaMap
  calleeStack <-setRange calleeStack maxArgCount fixedArguments  -- setStackSize maxArgCount $ pushObjects (createStack 0) fixedArguments
  let newExecutionThread = LuaExecutionThread calleeInstance oldExecutionThread 0 LuaStateRunning (LuaCallInfo varArgs)

  ---call resulting function
  let newState = LuaState newExecutionThread $ lGetGlobals state
  updateStack newState (return . const calleeStack)



-- Collects the number of parameters given by rb in the call instruction
-- offset gives the position of the first parameter, rb the expected parameter count
collectValues :: (LuaStack a) => Int -> Int -> a -> IO [LuaObject]
collectValues offset rbCount stack
  | rbCount == 1 = return []
  | rbCount > 1 = getRange stack offset (offset + rbCount -2)
  | rbCount == 0 = do ss <- stackSize stack; getRange stack offset (ss - 1)
  | otherwise = error "Invalid parameter count"

-- | When returning from a function we need to:
{-
a) Collect the values we want to return
b) Push the returned values on the callers stack
c) Reset the execution context back to the one of the caller
d) TODO: CLose upvalues
-}

returnCall :: IO LuaState -> IO LuaState
returnCall state = do
  returnInstruction <- fmap getInstruction state

  --collect results
  stack <- Monad.join $ fmap lGetStateStack state :: IO LuaMap
  results <- collectValues (LVM.ra returnInstruction) (LVM.rb returnInstruction) stack :: IO [LuaObject]
  --traceM $ "Results:" ++ show results

  prevExecInst <- fmap lGetLastFrame state

  --b - c are handled by this function
  s <- state

  returnByOrigin state prevExecInst results


returnByOrigin :: IO LuaState -> LuaExecutionThread -> [LuaObject] -> IO LuaState
--When returning to Haskell we only pass back the list of results
returnByOrigin state (LuaExecInstanceTop _) results = do
  --Trace.traceM "Returning to Haskell"
  globals <- Monad.liftM lGetGlobals state
  return $ LuaState (LuaExecInstanceTop results) globals

--Returning back to a caller lua function
returnByOrigin state exec results = do
  --traceIO "Returning to Lua Caller"
  (LuaState (LuaExecutionThread _ prevExecInst _ _ callInfo) globals) <- state

  --unsafeIOToST $ traceIO "Returning to Lua Caller"
  --Number of results to return based on the previous call code, 0 variable, 1 = none, > 1 = n - 1
  resultCount <- LVM.rc . getInstruction <$> state
  --unsafeIOToST $ traceIO $ show resultCount ++ " results"
  let createdResults = if resultCount == 0 then results else take (resultCount - 1) results

  --Update the previous stack frame, remove parameters not requrested
  let state = LuaState prevExecInst globals
  let requestCount = LVM.rc . getInstruction $ state
  let returnedResults = if requestCount == 0 then createdResults else take (requestCount - 1) createdResults

  let newState = updateStack (LuaState prevExecInst globals) $ flip pushObjects returnedResults
  incPC <$> newState

-- pc points to the tailcall function
-- replace current execution Thread by one execution the callee
tailCall :: IO LuaState -> IO LuaState
tailCall state = do
  (callee@(LOFunction calledFunction), parameters) <- Monad.join $ fmap getCallArguments state :: IO (LuaObject, [LuaObject])
  --If we have a tailcall to a non lua function something REALLY went wrong, so guarantee this with an assert

  x <- Monad.join $ isLuaCall <$> state :: IO Bool
  return $ assert x ()

  let lof@(LOFunction calledFunction) = callee

  let newStackSize = lgetMaxStackSize lof :: Int

  --collect  parameters
  let maxArgCount = lgetArgCount lof :: Int
  let (fixedArgs, varArgs) = splitAt maxArgCount parameters
  newStack <- fromList fixedArgs :: IO LuaMap --Monad.join $ flip setStackSize newStackSize $ flip pushObjects fixedArgs $ createStack 0 :: IO LuaMap

  prevExecInst <- fmap lGetLastFrame state
  globals <- fmap lGetGlobals state
  let newState = LuaState (LuaExecutionThread calledFunction prevExecInst 0 LuaStateRunning (LuaCallInfo varArgs)) globals

  --updateStack fails here
  updateStack newState $ return . const newStack


concatOP :: (LuaStack a) => a -> Int -> Int -> IO LuaObject
concatOP stack from to =
  case compare from to of
    Prelude.GT -> return $ LOString ""
    otherwise -> do
      x <- concat <$> mapM
        (\i -> do e <- getElement stack i; return (init . loString $ ltoString e))
        [from..to]

      return $ LOString x -- $ traceShowId $ start ++ next


printStack :: LuaState -> IO ()
printStack state = do
  print "Stack:"
  let s = state
  ps s
  where
    ps (LuaState (LuaExecInstanceTop res) _) =
      mapM_ print res
    ps state = do
      stack <- lGetStateStack state --let (LuaState (LuaExecutionThread LuaFunctionInstance {funcStack = stack} prevInst pc execState callInfo) globals) = state
      ss <- stackSize stack
      Foldable.traverse_ (\n -> do e <- getElement stack n; print e) [0..ss - 1]

stackWalk :: LVM.LuaState -> IO ()
stackWalk state = do
  print "Stackwalk"
  ps state
  return ()
  where
    ps (LVM.LuaState (LuaObjects.LuaExecInstanceTop res) _) =
      mapM_ print res
    ps state = do
      stack <- lGetStateStack state --let (LVM.LuaState (LuaExecutionThread LuaFunctionInstance {funcStack = stack } prevInst pc execState callInfo) globals) = state
      let prevInst = execPrevInst $ stateExecutionThread state
      ss <- stackSize stack
      Foldable.traverse_ (fmap print . getElement stack) [0..ss - 1]
      print "1-UP"
      ps $ LVM.LuaState prevInst Map.empty
