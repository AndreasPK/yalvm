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

lGetStateStack :: LuaState -> LuaMap
lGetStateStack (LuaState executionThread@(LuaExecutionThread (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) prevExecInst pc execState callInfoEmpty) globals) =
  stack
lGetStateStack (LuaState (LuaExecInstanceTop results) _ ) = fromList results

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
updateStack :: LuaState -> (LuaMap -> LuaMap) -> LuaState
updateStack (LuaState (LuaExecutionThread (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) prevInst pc execState callInfo) globals) updateFunction =
  LuaState (LuaExecutionThread (LuaFunctionInstance newStack instructions constList funcPrototype varargs upvalues) prevInst pc execState callInfo) globals
  where
    newStack = updateFunction stack :: LuaMap
updateStack (LuaState (LuaExecutionThread (HaskellFunctionInstance name stack f) prevInst pc execState callInfo) globals) updateFunction =
  LuaState (LuaExecutionThread (HaskellFunctionInstance name (updateFunction stack) f) prevInst pc execState callInfo) globals

getInstruction :: LuaState -> LuaInstruction
getInstruction (LuaState (LuaExecutionThread (LuaFunctionInstance _ instructions _ _ _ _) _ pc _ _) _) =
  instructions !! pc

--get the instruction at pc + offset
getRelativeInstruction :: LuaState -> Int -> LuaInstruction
getRelativeInstruction state offset =
  let func = execFunctionInstance . stateExecutionThread $ state
  in
  (funcInstructions func !! (getPC state + offset))


-- | Program counter manipulation
setPC :: LuaState -> Int -> LuaState
setPC (LuaState (LuaExecutionThread (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) prevInst pc execState callInfo) globals) newipc =
  LuaState (LuaExecutionThread (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) prevInst newipc execState callInfo) globals

getPC :: LuaState -> Int
getPC (LuaState (LuaExecutionThread (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) prevInst pc execState callInfo) globals) = pc

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
  let  LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues = functionInst
       LuaMap stackMap = stack
       nextInstruction = instructions !! pc
       opCode = LuaLoader.op nextInstruction :: LuaOPCode
       ra = LuaLoader.ra nextInstruction :: Int
       rb = LuaLoader.rb nextInstruction
       rc = LuaLoader.rc nextInstruction
       rbx = LuaLoader.rbx nextInstruction
       rsbx = LuaLoader.rsbx nextInstruction

       --For K enccoding bit 9 deternines if we use a constant or a stack value
       decodeConst :: Int -> LuaObject
       decodeConst register = if (register Bits..&. 256) == 256 then
         getConst constList (register - 256) else
           getElement stack register

  let instruction = getInstruction state
      opCode = LuaLoader.op instruction
  putStrLn $ show (getPC state + 1) ++ ":" ++ ppLuaInstruction instruction

  case opCode of
    MOVE -> -- RA = RB
      return $ incPC $ updateStack state (\stack ->setElement stack ra $ getElement stack rb)
    LOADNIL ->
      return $ incPC $ updateStack state (\(LuaMap m) -> LuaMap $ foldl (\m k -> Map.insert k LONil m) stackMap [ra..rb])
    LOADK ->
      return $ incPC $ updateStack state (\stack -> setElement stack ra $ getConst constList rbx)
    LOADBOOL ->
      let value = LOBool $ rb /= 0 :: LuaObject
      in
      return $ incPC $ flip updateStack (\stack -> setElement stack ra value) $ --set boolean value
        if rc /= 0 then incPC state else state --increment pc based on rc(or not)
    GETGLOBAL -> -- R(A) := Glb(Kst(rbx))
      let LOString s = getConst constList rbx :: LuaObject
          value = fromMaybe (error $ "Global doesn't exist!!!" ++ show s ++ show globals) $ Map.lookup s globals
      in
      return $ incPC $ updateStack state (\stack -> setElement stack ra value)
    SETGLOBAL ->
      let LOString s = getConst constList rbx :: LuaObject
          value = getElement stack ra
      in
      return $ incPC $ LuaState executionThread $ Map.insert s value globals
    GETUPVAL ->
      let value = getUpvalue upvalues rb :: LuaObject
      in
      return $ incPC $ updateStack state (\s -> setElement s ra value)
    SETUPVAL ->
      let value = getElement stack ra
          newUpvalues = setUpvalue upvalues rb value
      in
      return $ incPC $ LuaState (LuaExecutionThread (LuaFunctionInstance stack instructions constList funcPrototype varargs newUpvalues) prevInst pc execState callInfo) globals
    GETTABLE ->
      let index = decodeConst rc
          LOTable table = getElement stack rb :: LuaObject
          value = getTableElement table index
      in
      traceShow(table, value, index)
      return $ incPC $ updateStack state $ \s -> setElement s ra value
    SETTABLE ->
      let value = decodeConst rc
          LOTable table = getElement stack ra
          newTable = LOTable $ setTableElement table (decodeConst rb) value
      in
      --traceShow (ra, table, newTable)
      return $ incPC $ updateStack state $ \s -> setElement s ra newTable
    ADD ->
      let x = ltoNumber $ decodeConst rb
          y = ltoNumber $ decodeConst rc
      in
      return $ incPC $ updateStack state (\s -> setElement s ra $ ladd x y)
    SUB ->
      let x = ltoNumber $ decodeConst rb
          y = ltoNumber $ decodeConst rc
      in
      return $ incPC $ updateStack state (\s -> setElement s ra $ lsub x y)
    MUL ->
      let x = ltoNumber $ decodeConst rb
          y = ltoNumber $ decodeConst rc
      in
      return $ incPC $ updateStack state (\s -> setElement s ra $ lmul x y)
    DIV ->
      let x = ltoNumber $ decodeConst rb
          y = ltoNumber $ decodeConst rc
      in
      return $ incPC $ updateStack state (\s -> setElement s ra $ ldiv x y)
    MOD ->
      let x = ltoNumber $ decodeConst rb
          y = ltoNumber $ decodeConst rc
      in
      return $ incPC $ updateStack state (\s -> setElement s ra $ lmod x y)
    POW ->
      let x = ltoNumber $ decodeConst rb
          y = ltoNumber $ decodeConst rc
      in
      return $ incPC $ updateStack state (\s -> setElement s ra $ lpow x y)
    UNM ->
      let LONumber x = ltoNumber $ getElement stack rb
      in
      return $ incPC $ updateStack state (\s -> setElement stack ra (LONumber (-x)))
    NOT ->
      let LOBool value = ltoBool $ getElement stack rb --Convert RB to boolean value
          raVal = if value then LOBool False else LOBool True --invert value
      in
      return $ incPC $ updateStack state (\stack -> setElement stack ra raVal)
    LEN ->
      return $ incPC $ updateStack state (\stack -> setElement stack ra $ lLen $ getElement stack rb)
    CONCAT ->
      return $ incPC $ updateStack state (\stack -> setElement stack ra $ concatOP stack rb rc)
    JMP ->
      return $ incPC $ setPC state $ rsbx + getPC state
    CLOSURE ->
      return $ incPC $ execCLOSURE state ra rbx
    VARARG ->
      return $ incPC $ updateStack state (\s -> updateVarArg s callInfo ra rb)
    SELF ->
      let index = decodeConst rc
          table = getElement stack rb
          callee = (\(LOTable t) -> getTableElement t index) table :: LuaObject
          update = (\s -> setElement s ra callee) . (\s -> setElement s (ra+1) table) :: (LuaStack s) => s -> s
      in
      return $ incPC $ updateStack state update
    LuaLoader.EQ ->
      let a = decodeConst rb
          b = decodeConst rc
          comparison = a == b
          result     = not $ compBtoI comparison ra
      in  return $ incPC $ if result then incPC state
          else state
    LuaLoader.LT ->
      let a = decodeConst rb
          b = decodeConst rc
          comparison = a < b
          result     = not $ compBtoI comparison ra
      in  return $ incPC $ if result then incPC state
          else state
    LuaLoader.LE ->
      let a = decodeConst rb
          b = decodeConst rc
          comparison = a <= b
          result     = not $ compBtoI comparison ra
      in  return $ incPC $ if result then incPC state
          else state
    TEST ->
      let (LOBool a) = ltoBool $ getElement stack ra :: LuaObject
          match = compBtoI a rc
      in
      return $ incPC $ if not match then incPC state
        else state
    TESTSET ->
      let (LOBool b) = ltoBool $ getElement stack rb :: LuaObject
          match = compBtoI b rc
      in
      return $ incPC $ if match then updateStack state (\stack -> setElement stack ra $ getElement stack rb)
        else incPC state
    FORPREP ->
      let step = getElement stack $ ra+2
          index = lsub (getElement stack ra) step
      in
      return $ incPC $ setPC
        (updateStack state $ const $ setElement stack ra index) --Update ra in the stack
        (getPC state + rsbx)
    FORLOOP ->
      let index = ladd (getElement stack $ ra + 2) (getElement stack ra)
          limit = getElement stack $ ra + 1
          stepping = getElement stack $ ra + 2
          comparison = if lvNumber stepping >= 0 then (<=) else (>=) :: Double -> Double -> Bool --Check depends on sign of stepping
          check = Data.Function.on comparison lvNumber -- (\a b -> comparison (lvNumber a) (lvNumber b))
          newState = updateStack state $ \s -> setElement s ra index
      in
      return $ incPC $ if check index limit then
        setPC
          (updateStack newState $ \s -> setElement s (ra+3) index )
          (rsbx + getPC state)
          else state

    TFORLOOP ->
      error "TFORLOOP Undefined"
    NEWTABLE ->
      return $ incPC $ updateStack state (\stack -> setElement stack ra $ LOTable createTable)
    SETLIST ->
      -- used to set an array of elements at once
      let table = getElement stack ra
          elementCount = if rb > 0 then rb else stackSize stack - (ra + 1)
          elements = getRange stack (ra+1) (ra+elementCount) :: [LuaObject]
          index = if rc /= 0 then (rc - 1) * 50 + 1 else error "large indices not supported"
          newPairs = zip (map (LONumber . fromIntegral) [index ..]) elements
          newTable = foldl (\(LOTable t) (k, v) -> LOTable $ setTableElement t k v) table newPairs
      in
      return $ incPC $ updateStack state (\s -> setElement s ra newTable)
    CALL -> runCall $ return state
    TAILCALL -> tailCall $ return state
    RETURN -> returnCall $ return state

    otherwise -> error "Unknown OP-Code"


compBtoI :: Bool -> Int -> Bool
compBtoI b i = b && i > 0 || not b && i == 0

--copies values from vararg list to the stack, might update top of stack
updateVarArg :: (LuaStack a) => a -> LuaCallInfo -> Int -> Int -> a
updateVarArg stack (LuaCallInfo varargs) offset countIndicator
  | countIndicator == 0 = foldl (\s (k, v) -> setElement s k v) stack $ zip [offset .. ] varargs
  | countIndicator > 1 = foldl (\s (k, v) -> setElement s k v) stack $ zip [offset .. (offset + countIndicator - 2)] varargs

--create a new closure
execCLOSURE :: LuaState -> Int -> Int -> LuaState
execCLOSURE state@(LuaState executionThread globals) ra rbx =
  let funcPrototype@(Parser.LuaFunctionHeader _ _ _ upvalueCount paramCount varargFlag stackSize instList constList _ _ _ _) = getContainedFunctionHeader executionThread rbx :: Parser.LuaFunctionHeader
      LOFunction func = linstantiateFunction funcPrototype :: LuaObject
      pc = getPC state
      upvalueInstructions = fmap (getRelativeInstruction state) [1 .. fromIntegral upvalueCount] :: [LuaInstruction]
      func1 = func { funcUpvalues = LuaRTUpvalueList undefined}

      --TODO: create upvalues
  in
  flip setPC (pc + fromIntegral upvalueCount) $ updateStack state (\stack -> setElement stack ra (LOFunction func1))

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
getCallee :: LuaState -> LuaObject
getCallee state =
  let inst = getInstruction state
      ra = LVM.ra inst
      stack = lGetStateStack state
      callee = getElement stack ra
  in callee

isLuaCall :: LuaState -> Bool
isLuaCall state =
  let (LOFunction x) = getCallee state
  in
  ft x
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
  if isLuaCall s then runLuaCall state else runHaskellCall state

-- | returns the called function and its parameters, ra is callee function
--   rb encodes the number of parameters
--   rc encodes the number of return values (not used)
getCallArguments :: LuaState -> (LuaObject, [LuaObject])
getCallArguments state =
  let callInst = getInstruction state -- Call instruction
      calleePosition = LVM.ra callInst
      stack = lGetStateStack state
      parameters = collectValues (calleePosition +1) (LVM.rb callInst) stack
      callee = getCallee state
  in
  (callee, parameters)

runHaskellCall :: IO LuaState -> IO LuaState
runHaskellCall state = do
  (callee, parameters) <- fmap getCallArguments state
  let LOFunction (HaskellFunctionInstance name _s func) = callee


  --Call haskell function with arguments, transform results back to State
  results <- func $ return (fromList parameters :: LuaMap)

  --remove parameters + function from caller stack
  let stackUpdate s = shrink s $ length parameters
  state <- (`updateStack` stackUpdate) <$> state

  returnFromHaskell (return state) (toList results)

returnFromHaskell :: IO LuaState -> [LuaObject] -> IO LuaState
returnFromHaskell state results = do
  inst <- getInstruction <$> state
  let requestCount = LVM.rc inst
  results <- return $ if requestCount == 0 then results else take (requestCount-1) results

  let stackUpdate s = setRange s (LVM.ra inst) results
  incPC . (`updateStack` stackUpdate) <$> state

runLuaCall :: IO LuaState -> IO LuaState
runLuaCall state = do
  state <- state
  let (LuaState executionThread@(LuaExecutionThread functionInst prevInst pc execState callInfo) globals) = state
  let (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) = functionInst

  let (calledFunction, parameters) = getCallArguments state
  let calleeInstance = (\(LOFunction f) -> f) calledFunction :: LuaFunctionInstance
  let calleeHeader = lgetFunctionHeader calledFunction :: Parser.LuaFunctionHeader
  --collect arguments
  let maxArgCount = lgetArgCount calledFunction :: Int

  --clearing caller stack
  let argCount = length parameters :: Int

  --remove objects used as parameters from the callers stack (shrink stack by count +1)
  let nos@(LuaState oldExecutionThread _) = updateStack state (\s -> setStackSize s $ stackSize s - (max argCount maxArgCount + 1))

  --split arguments in fixed args and varargs
  let (fixedArguments, varArgs) = splitAt maxArgCount parameters :: ([LuaObject], [LuaObject])

  let calleeStack = flip setStackSize maxArgCount $ pushObjects (createStack 0) fixedArguments
  let newExecutionThread = LuaExecutionThread calleeInstance oldExecutionThread 0 LuaStateRunning (LuaCallInfo varArgs)

  ---call resulting function
  return $ flip updateStack (const calleeStack) $ LuaState newExecutionThread $ lGetGlobals state



-- Collects the number of parameters given by rb in the call instruction
-- offset gives the position of the first parameter, rb the expected parameter count
collectValues :: (LuaStack a) => Int -> Int -> a -> [LuaObject]
collectValues offset rbCount stack
  | rbCount == 1 = []
  | rbCount > 1 = getRange stack offset (offset + rbCount -2)
  | rbCount == 0 = getRange stack offset (stackSize stack - 1)
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
  stack <- fmap lGetStateStack state
  let results = collectValues (LVM.ra returnInstruction) (LVM.rb returnInstruction) stack :: [LuaObject]
  --traceM $ "Results:" ++ show results

  prevExecInst <- fmap lGetLastFrame state

  --b - c are handled by this function
  s <- state

  returnByOrigin state prevExecInst results


returnByOrigin :: IO LuaState -> LuaExecutionThread -> [LuaObject] -> IO LuaState
--When returning to Haskell we only pass back the list of results
returnByOrigin state (LuaExecInstanceTop undefined) results = do
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
  return $ incPC newState

-- pc points to the tailcall function
-- replace current execution Thread by one execution the callee
tailCall :: IO LuaState -> IO LuaState
tailCall state = do
  (callee@(LOFunction calledFunction), parameters) <- fmap getCallArguments state
  --If we have a tailcall to a non lua function something REALLY went wrong, so guarantee this with an assert
  do
    x <- fmap isLuaCall state
    return $ assert x ()

  let lof@(LOFunction calledFunction) = callee

  let newStackSize = lgetMaxStackSize lof :: Int

  --collect  parameters
  let maxArgCount = lgetArgCount lof :: Int
  let (fixedArgs, varArgs) = splitAt maxArgCount parameters
  let newStack = flip setStackSize newStackSize $ flip pushObjects fixedArgs $ createStack 0

  prevExecInst <- fmap lGetLastFrame state
  globals <- fmap lGetGlobals state
  let newState = LuaState (LuaExecutionThread calledFunction prevExecInst 0 LuaStateRunning (LuaCallInfo varArgs)) globals

  --updateStack fails here
  return $ updateStack newState $ const newStack


concatOP :: (LuaStack a) => a -> Int -> Int -> LuaObject
concatOP stack from to
  | from > to = LOString ""
  | otherwise =
      --traceShow start
      LOString $ start ++ next
  where LOString start = ltoString $ getElement stack from :: LuaObject
        LOString next = concatOP stack (from+1) to

printStack :: LuaState -> IO ()
printStack state = do
  print "Stack:"
  let s = state
  ps s
  where
    ps (LuaState (LuaExecInstanceTop res) _) =
      mapM_ print res
    ps state = do
      let (LuaState (LuaExecutionThread (LuaFunctionInstance stack _ _ _ _ _) prevInst pc execState callInfo) globals) = state
      Monad.foldM (\_ n -> print $ getElement stack n) () [0..stackSize stack - 1]

stackWalk :: LVM.LuaState -> IO ()
stackWalk state = do
  print "Stackwalk"
  ps state
  return ()
  where
    ps (LVM.LuaState (LuaObjects.LuaExecInstanceTop res) _) =
      mapM_ print res
    ps state = do
      let (LVM.LuaState (LuaExecutionThread (LuaFunctionInstance stack _ _ _ _ _) prevInst pc execState callInfo) globals) = state
      Monad.foldM (\_ n -> print $ getElement stack n) () [0..stackSize stack - 1]
      print "1-UP"
      ps $ LVM.LuaState prevInst undefined
