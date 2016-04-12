module LVM where

import LuaLoader
import LuaObjects
import qualified Data.Map.Strict as Map
import Control.Monad.ST
import Data.Either
import Data.STRef
import Data.Array.IArray as Array
import qualified Data.Bits as Bits
import System.IO
import Data.IORef
import Debug.Trace
import qualified Parser
import qualified Control.Monad as Monad

type LuaGlobals = Map.Map String LuaObject

-- | Fully describes the state of the virtual machine
data LuaState = LuaState
  LuaExecutionThread        --current execution Thread
  (Map.Map String LuaObject) --Global list
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

stackChange :: LuaFunctionInstance -> LuaMap -> LuaFunctionInstance
stackChange (LuaFunctionInstance _ instructions constList funcPrototype varargs upvalues) stack =
  LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues

-- | Applies the given function to the Lua stack
updateStack :: LuaState -> (LuaMap -> LuaMap) -> LuaState
updateStack (LuaState (LuaExecutionThread (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) prevInst pc execState callInfo) globals) updateFunction =
  LuaState (LuaExecutionThread (LuaFunctionInstance newStack instructions constList funcPrototype varargs upvalues) prevInst pc execState callInfo) globals
  where
    newStack = updateFunction stack :: LuaMap

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
--  Only executes "pure" changes
execPureOP :: LuaState -> LuaState
execPureOP
  state@(LuaState executionThread@(LuaExecutionThread functionInst prevInst pc execState callInfo) globals)
  | opCode == MOVE = -- RA = RB
      updateStack state (\stack ->setElement stack ra $ getElement stack rb)
  | opCode == LOADNIL =
      updateStack state (\(LuaMap m) -> LuaMap $ foldl (\m k -> Map.insert k LONil m) stackMap [ra..rb])
  | opCode == LOADK =
      updateStack state (\stack -> setElement stack ra $ getConst constList rbx)
  | opCode == LOADBOOL =
      let bv = if rb == 0 then LOBool False else LOBool True :: LuaObject
          offset = if rc == 0 then 0 else 1 :: Int
      in
      setPC
        (updateStack state (\stack -> setElement stack ra bv))
        offset
  | opCode == GETGLOBAL = -- R(A) := Glb(Kst(rbx))
      let LOString s = getConst constList rbx :: LuaObject
          Just value = Map.lookup s globals
      in
      updateStack state (\stack -> setElement stack ra value)
  | opCode == SETGLOBAL =
      let LOString s = getConst constList rbx :: LuaObject
          value = getElement stack ra
      in
      LuaState executionThread $ Map.insert s value globals
  | opCode == GETUPVAL =
      let value = getUpvalue upvalues rb :: LuaObject
      in
      updateStack state (\s -> setElement s ra value)
  | opCode == SETUPVAL =
      let value = getElement stack ra
          newUpvalues = setUpvalue upvalues rb value
      in
      LuaState (LuaExecutionThread (LuaFunctionInstance stack instructions constList funcPrototype varargs newUpvalues) prevInst pc execState callInfo) globals
  | opCode == GETTABLE =
    let LOTable table = getElement stack rb :: LuaObject
        value = getTableElement table $ decodeConst rc
    in
    updateStack state (\s -> setElement s ra value)
  | opCode == SETTABLE =
    let value = getElement stack rc
        LOTable table = getElement stack ra
        newTable = LOTable $ setTableElement table (decodeConst rb) value
    in
    updateStack state (\s -> setElement s ra newTable)
  | opCode == ADD =
    let x = ltoNumber $ decodeConst rb
        y = ltoNumber $ decodeConst rc
    in
    updateStack state (\s -> setElement s ra $ ladd x y)
  | opCode == SUB =
    let x = ltoNumber $ decodeConst rb
        y = ltoNumber $ decodeConst rc
    in
    updateStack state (\s -> setElement s ra $ lsub x y)
  | opCode == MUL =
    let x = ltoNumber $ decodeConst rb
        y = ltoNumber $ decodeConst rc
    in
    updateStack state (\s -> setElement s ra $ lmul x y)
  | opCode == DIV =
    let x = ltoNumber $ decodeConst rb
        y = ltoNumber $ decodeConst rc
    in
    updateStack state (\s -> setElement s ra $ ldiv x y)
  | opCode == MOD =
    let x = ltoNumber $ decodeConst rb
        y = ltoNumber $ decodeConst rc
    in
    updateStack state (\s -> setElement s ra $ lmod x y)
  | opCode == POW =
    let x = ltoNumber $ decodeConst rb
        y = ltoNumber $ decodeConst rc
    in
    updateStack state (\s -> setElement s ra $ lpow x y)
  | opCode == UNM =
    let LONumber x = ltoNumber $ getElement stack rb
    in
    updateStack state (\s -> setElement stack ra (LONumber (-x)))
  | opCode == NOT =
    let LOBool value = ltoBool $ getElement stack rb --Convert RB to boolean value
        raVal = if value then LOBool False else LOBool True --invert value
    in
    updateStack state (\stack -> setElement stack ra raVal)
  | opCode == LEN =
    updateStack state (\stack -> setElement stack ra $ lLen $ getElement stack rb)
  | opCode == CONCAT =
    updateStack state (\stack -> setElement stack ra $ concatOP stack rb rc)
  | opCode == JMP =
    setPC state $ rsbx + getPC state
  | opCode == CLOSURE =
      execCLOSURE state ra rbx
  | otherwise =
      state
  where
     LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues = functionInst
     LuaMap stackMap = stack
     nextInstruction = instructions !! pc
     opCode = LuaLoader.op nextInstruction :: LuaOPCode
     ra = LuaLoader.ra nextInstruction :: Int
     rb = LuaLoader.rb nextInstruction
     rc = LuaLoader.rc nextInstruction
     rbx = LuaLoader.rbx nextInstruction
     rsbx = LuaLoader.rsbx nextInstruction
     decodeConst :: Int -> LuaObject
     decodeConst register = if (register Bits..&. 256) == 256 then
       getConst constList (register - 256) else
         getElement stack register

execCLOSURE :: LuaState -> Int -> Int -> LuaState
execCLOSURE state@(LuaState executionThread globals) ra rbx =
  let funcPrototype@(Parser.LuaFunctionHeader _ _ _ upvalueCount paramCount varargFlag stackSize instList constList _ _ _ _) = getContainedFunctionHeader executionThread rbx :: Parser.LuaFunctionHeader
      func = linstantiateFunction funcPrototype :: LuaObject
      pc = getPC state
      --TODO: create upvalues
  in
  flip setPC (pc + fromIntegral upvalueCount) $ updateStack state (\stack -> setElement stack ra func)

--Continue stepping the VM until we reach a return statement
runLuaFunction :: IO LuaState -> IO LuaState
runLuaFunction state = do
  state <- state
  state <- return $ execPureOP state --execute simple op codes
  let (LuaState executionThread@(LuaExecutionThread functionInst _prev_func pc execState callInfo) globals) = state
  let (LuaFunctionInstance stack instructions constList funcPrototype _varargs upvalues) = functionInst
  let nextInstruction = instructions !! pc
--  let calledFunction = getElement stack (LVM.ra nextInstruction) :: LuaObject
  let opCode = LuaLoader.op nextInstruction
  putStrLn ""
  print $ ppLuaInstruction nextInstruction
  printStack $ return state
  case opCode of
    --We increment the callers pc, then we change the context with runCall before tail-calling back to runLuaFunction
    CALL -> runLuaFunction $ runCall $ return $ incPC state
    RETURN -> returnCall $ return state
    otherwise -> runLuaFunction $ return $ incPC state --Execute next instruction in this function

-- |  We enter the function with the ip being at the instruction after the call
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
  state <- state
  let (LuaState executionThread@(LuaExecutionThread functionInst prevInst pc execState callInfo) globals) = state
  let (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) = functionInst
  let callInst = instructions !! (pc-1) -- Call instruction
  let calleePosition = LVM.ra callInst
  let calledFunction = getElement stack calleePosition :: LuaObject
  let calleeInstance = (\(LOFunction f) -> f) calledFunction :: LuaFunctionInstance
  let calleeHeader = lgetFunctionHeader calledFunction :: Parser.LuaFunctionHeader

  --collect arguments
  let maxArgCount = lgetArgCount calledFunction :: Int
  let passedArguments = collectValues (calleePosition +1) (LVM.rb callInst) stack :: [LuaObject]

  --print $ "we are passing these arguments:" ++ show passedArguments

  --clearing caller stack
  let argCount = length passedArguments :: Int
  let nos@(LuaState oldExecutionThread _) = updateStack state (\s -> setStackSize s $ stackSize s - (max argCount maxArgCount + 1)) --shrink stack by parameter count +1 for the passed function

  --putStrLn "\nNew old stack:"
  --traceIO $ "Argument counts and shit" ++ show (argCount, maxArgCount, passedArguments)
  --printStack $ return nos

  --split arguments in fixed args and varargs
  let (fixedArguments, varArgs) = splitAt maxArgCount passedArguments :: ([LuaObject], [LuaObject])

  let calleeStack = flip setStackSize maxArgCount $ pushObjects (createStack 0) fixedArguments
  ---call resulting function
  return $ updateStack (flip LuaState globals $ LuaExecutionThread calleeInstance oldExecutionThread 0 LuaStateRunning (LuaCallInfo varArgs))
                       (const calleeStack)

-- Collects the number of parameters given by rb in the call instruction
-- offset gives the position of the first parameter, rb the expected parameter count
collectValues :: (LuaStack a) => Int -> Int -> a -> [LuaObject]
collectValues offset rbCount stack
  | rbCount == 1 = []
  | rbCount > 1 = getRange stack offset (offset + rbCount -2)
  | rbCount == 0 = getRange stack offset (stackSize stack - 1)
  | otherwise = undefined


-- | When returning from a function we need to:
{-
a) Collect the values we want to return
b) Push the returned values on the callers stack
c) Reset the execution context back to the one of the caller
d) TODO: CLose upvalues
-}

returnCall :: IO LuaState -> IO LuaState
returnCall state = do
  --printStack state
  --print "before return"
  state <- state
  let oldState@(LuaState executionThread@(LuaExecutionThread functionInst prevExecInst pc execState callInfo) globals) = state
  let (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) = functionInst
  let returnInstruction = instructions !! pc

  --collect results
  let results = collectValues (LVM.ra returnInstruction) (LVM.rb returnInstruction) stack:: [LuaObject]

  {-
  putStrLn $ "\nResults for " ++ ppLuaInstruction returnInstruction ++ " are:"
  mapM_ print results
  putStrLn ""-}

  --b - c are handled by this function
  returnByOrigin state prevExecInst results


returnByOrigin :: LuaState -> LuaExecutionThread -> [LuaObject] -> IO LuaState
--When returning to Haskell we only pass back the list of results
returnByOrigin state (LuaExecInstanceTop _) results = do
  --putStrLn "Returning to Haskell"
  let (LuaState _ globals) = state
  return $ LuaState (LuaExecInstanceTop results) globals
--Returning back to a caller lua function
returnByOrigin state exec results = do
  --print exec
  --putStrLn "Returning to Lua Caller"
  let (LuaState (LuaExecutionThread _ prevExecInst _ _ callInfo) globals) = state
  let newState = updateStack (LuaState prevExecInst globals) $ flip pushObjects results
  --printStack $ return state
  --putStrLn ""
  --printStack $ return newState
  return newState



concatOP :: (LuaStack a) => a -> Int -> Int -> LuaObject
concatOP stack from to
  | from > to = LOString ""
  | otherwise =
      LOString $ start ++ next
  where LOString start = ltoString $ getElement stack from :: LuaObject
        LOString next = concatOP stack (from+1) to

printStack :: IO LuaState -> IO ()
printStack state = do
  s <- state
  ps s
  where
    ps (LuaState (LuaExecInstanceTop res) _) =
      mapM_ print res
    ps state = do
      let (LuaState (LuaExecutionThread (LuaFunctionInstance stack _ _ _ _ _) prevInst pc execState callInfo) globals) = state
      Monad.foldM (\_ n -> print $ getElement stack n) () [0..stackSize stack - 1]
