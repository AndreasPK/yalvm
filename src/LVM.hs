{-# LANGUAGE TypeSynonymInstances #-}

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
lGetStateStack :: LuaState -> IO LVStack
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
updateStack :: LuaState -> (LVStack -> IO LVStack) -> IO LuaState
updateStack state f =
  let exec = stateExecutionThread state
      func = execFunctionInstance exec
      stack = Monad.join $ f <$> funcStack func :: IO LVStack
  in
  return $ state { stateExecutionThread = exec { execFunctionInstance = func { funcStack = stack }}}



getInstruction :: LuaState -> LuaInstruction
getInstruction state  = --(LuaState (LuaExecutionThread (LuaFunctionInstance _ instructions _ _ _ _) _ pc _ _) _) =
  let exec = stateExecutionThread state
      pos = execCurrentPC exec
  in
  (funcInstructions . execFunctionInstance) exec V.! pos
  -- instructions !! pc

--get the instruction at pc + offset
getRelativeInstruction :: LuaState -> Int -> LuaInstruction
getRelativeInstruction state offset =
  let func = execFunctionInstance . stateExecutionThread $ state
  in
  (funcInstructions func V.! (getPC state + offset))


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
  --traceM $ show (instructions, pc)
  let  nextInstruction = instructions V.! pc
       opCode = LuaLoader.op nextInstruction :: LuaOPCode
       ra = LuaLoader.ra $! nextInstruction :: Int
       rb = LuaLoader.rb nextInstruction :: Int
       rc = LuaLoader.rc nextInstruction
       rbx = LuaLoader.rbx nextInstruction :: Int
       rsbx = LuaLoader.rsbx nextInstruction :: Int

  stack <- stack --Extract the stack out of the io monad

       --For K enccoding bit 9 deternines if we use a constant or a stack value
  let  decodeConst :: Int -> IO LuaObject
       decodeConst register = if (register Bits..&. 256) == 256 then
         return (getConst constList (register - 256)) else
           getElement stack register

  let instruction = getInstruction state
      opCode = LuaLoader.op instruction

--  putStrLn "\n"
  ss <- stackSize stack
  putStr $ "StackSize:" ++ show ss ++ " - "
  putStrLn $ show (getPC state + 1) ++ ":" ++ ppLuaInstruction instruction

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
    GETGLOBAL -> do -- R(A) := Glb(Kst(rbx))
      let key = loString $ getConst constList rbx :: String
          value = fromMaybe (error $ "Global doesn't exist!!!" ++ show key ++ show globals) $ Map.lookup key globals
      incPC <$> updateStack state (\stack -> setElement stack ra value)
    SETGLOBAL -> do
      let s = loString $ getConst constList rbx :: String
      value <- getElement stack ra
      traceM $ "set" ++  show (s, value)
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
      func1 = func { funcUpvalues = LuaRTUpvalueList IntMap.empty}

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

-- |Remove the functionInstance and arguments from the stack
--  Then run the haskell function passing the arguments. Afterwards
--  take the requested amount of results from the Haskell function and place
--  them on the stack of the caller
runHaskellCall :: IO LuaState -> IO LuaState
runHaskellCall state = do
  traceM "runHaskellCall"
  --Get function arguments
  (callee, parameters) <- getCallArguments =<< state :: IO (LuaObject, [LuaObject])
  let func = funcFunc . loFunction $ callee :: IO LVStack -> IO LVStack
  traceM $ show (loFunction callee, parameters)
  --Call haskell function with arguments, transform results back to State
  results <- func (fromList parameters :: IO LVStack)


  --Trim results according to rc
  rlist <- toList results :: IO [LuaObject]
  inst <- getInstruction <$> state
  let requestCount = LVM.rc inst
  results <- return $ if requestCount == 0 then rlist else take (requestCount-1) rlist


  let stackUpdate s = setRange s (LVM.ra inst) results
  s <- state
  rres <- updateStack s stackUpdate
  return $ incPC rres


runLuaCall :: IO LuaState -> IO LuaState
runLuaCall state = do
  (calledFunction, parameters) <- getCallArguments =<< state
  let callee = loFunction calledFunction
  let calleeHeader = funcHeader callee

  let fixedArgCount = fromIntegral $ Parser.fhParameterCount calleeHeader

  let (fixedArguments, varArgs) = splitAt fixedArgCount parameters :: ([LuaObject], [LuaObject])

  --Pad arguments with Nil of neccesary in order to pass at least 'fixedArgCount' many arguments
  fixedArguments <- return $ take fixedArgCount $ fixedArguments ++ repeat LONil :: IO [LuaObject]


  calleeStack <- createStack $ fromIntegral $ Parser.fhMaxStacksize calleeHeader :: IO LVStack
  calleeStack <- setRange calleeStack 0 fixedArguments

  callee <- return $ callee { funcStack = return calleeStack }

  --traceM "calleeStack"
  --mapM_ print =<< toList calleeStack

  oldExec <- stateExecutionThread <$> state
  let newExecutionThread = LuaExecutionThread callee oldExec 0 LuaStateRunning (LuaCallInfo varArgs)
  LuaState newExecutionThread <$> fmap lGetGlobals state



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

returnCall :: IO LuaState -> IO LuaState
returnCall state = do
  returnInstruction <- fmap getInstruction state
  --print $ ppLuaInstruction returnInstruction
  --collect results
  stack <- lGetStateStack =<< state :: IO LVStack
  results <- collectValues (LVM.ra returnInstruction) (LVM.rb returnInstruction) stack :: IO [LuaObject]
  traceM $ "returnCall - Results:" ++ show results

  prevExecInst <- lGetLastFrame <$>  state

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
returnByOrigin state exec calleeResults = do
  --The instruction
  --traceIO "returnByOrigin: Returning to Lua Caller"
  --(LuaState (LuaExecutionThread _ prevExecInst _ _ callInfo) globals) <- state

  returnInstruction <- getInstruction <$> state
  print $ ppLuaInstruction returnInstruction
  --Number of results to return based on the previous call code, 0 variable, 1 = none, > 1 = n - 1
  resultCount <- LVM.rb . getInstruction <$> state
  --traceIO $ show resultCount ++ "=RB"
  let results = if resultCount == 0 then calleeResults else take (resultCount - 1) $ calleeResults ++ repeat LONil
  let stackOffset = LVM.ra returnInstruction

  --Update the previous stack frame, remove parameters not requrested
  prevFrame <- lGetLastFrame <$> state
  globals <- stateGlobals <$> state
  let state = LuaState { stateExecutionThread = prevFrame, stateGlobals = globals }


  let callInstruction = getInstruction state
  let callResultCount = LVM.rc callInstruction
  let resultPlacement = LVM.ra callInstruction
  --traceM $ "callInstruction: " ++ ppLuaInstruction callInstruction
  let callResults = if callResultCount == 0 then results else take (callResultCount - 1) $ results ++ repeat LONil


  newState <- updateStack state $ \s -> setRange s resultPlacement callResults --pushObjects returnedResults
  s <- toList =<< lGetStateStack newState
  --traceM $ "New stack:" ++ show s

  return $ incPC newState

-- pc points to the tailcall function
-- replace current execution Thread by one execution the callee
tailCall :: IO LuaState -> IO LuaState
tailCall state = do
  (callee@(LOFunction calledFunction), parameters) <- Monad.join $ fmap getCallArguments state :: IO (LuaObject, [LuaObject])
  --If we have a tailcall to a non lua function something REALLY went wrong, so guarantee this with an assert

  x <- isLuaCall =<< state :: IO Bool
  return $ assert x ()

  let lof@(LOFunction calledFunction) = callee

  let newStackSize = lgetMaxStackSize lof :: Int

  --collect  parameters
  let maxArgCount = lgetArgCount lof :: Int
  let (fixedArgs, varArgs) = splitAt maxArgCount parameters
  newStack <- createStack $ lgetMaxStackSize lof
  newStack <- setRange newStack 0 fixedArgs
  --newStack <- fromList fixedArgs :: IO LVStack --Monad.join $ flip setStackSize newStackSize $ flip pushObjects fixedArgs $ createStack 0 :: IO LuaMap

  prevExecInst <- fmap lGetLastFrame state
  globals <- fmap lGetGlobals state
  let newState = LuaState (LuaExecutionThread calledFunction prevExecInst 0 LuaStateRunning (LuaCallInfo varArgs)) globals

  --updateStack fails here?!?
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
  print "printStack:"
  let s = state
  ps s
  where
    ps (LuaState (LuaExecInstanceTop res) _) =
      mapM_ print res
    ps state = do
      stack <- lGetStateStack state --let (LuaState (LuaExecutionThread LuaFunctionInstance {funcStack = stack} prevInst pc execState callInfo) globals) = state
      ss <- stackSize stack
      Foldable.traverse_ (\n -> do e <- getElement stack n; putStrLn $ show n ++ ":" ++ show e) [0..ss - 1]

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
