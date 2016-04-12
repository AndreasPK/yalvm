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
  print $ ppLuaInstruction nextInstruction
  case opCode of
    CALL -> runCall $ return $ incPC state
    RETURN -> returnCall $ return state
    otherwise -> runLuaFunction $ return $ incPC state --Execute next instruction in this function

-- |  We enter the function with the ip being at the instruction after the call
--    and the execution frame still being that of the caller.
--


--sets up stack for function and then runs runLuaFunction on the function
--delets all passed parameters from the functions stack before calling
runCall :: IO LuaState -> IO LuaState
runCall state = do
  state <- state
  let (LuaState executionThread@(LuaExecutionThread functionInst prevInst pc execState callInfo) globals) = state
  let (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) = functionInst
  let callInst = instructions !! (pc-1) -- Call instruction
  let calledFunction = getElement stack (LVM.ra callInst) :: LuaObject
  let calleeInstance = (\(LOFunction f) -> f) calledFunction :: LuaFunctionInstance
  let arguments = getElements stack (LVM.rb callInst) (LVM.rb callInst) :: [LuaObject] --TODO: Get right number of arguments

  runLuaFunction $ return $ updateStack (flip LuaState globals $ LuaExecutionThread calleeInstance executionThread 0 LuaStateRunning callInfo)
                       (const $ LuaMap $ Map.fromList $ zip [0..] arguments)





-- when hitting a return in the code, switch back to previous functionInstance, adjust stack of origin function
returnCall :: IO LuaState -> IO LuaState
returnCall state = do
  printStack state
  print "before return"
  state <- state
  let oldState@(LuaState executionThread@(LuaExecutionThread functionInst prevExecInst pc execState callInfo) globals) = state
  let (LuaFunctionInstance stack instructions constList funcPrototype varargs upvalues) = functionInst
  let nextInstruction = instructions !! pc
  let results = getElements stack (LVM.ra nextInstruction) (LVM.rb nextInstruction) :: [LuaObject]

  putStrLn "\nResults are:"
  mapM_ print results
  putStrLn ""

  returnByOrigin state prevExecInst results


returnByOrigin :: LuaState -> LuaExecutionThread -> [LuaObject] -> IO LuaState
returnByOrigin state (LuaExecInstanceTop _) results = do
  putStrLn "Returning to Haskell"
  let (LuaState _ globals) = state
  return $ LuaState (LuaExecInstanceTop results) globals
returnByOrigin state exec results = do
  --print exec
  putStrLn "Returning to Lua Caller"
  let (LuaState (LuaExecutionThread _ prevExecInst _ _ callInfo) globals) = state
  return $ updateStack (LuaState prevExecInst globals)
    (\(LuaMap m) ->
      LuaMap $ foldl (\m (k,v) -> Map.insert k v m) m (zip [(Map.size m)..] results)
    )







-- |Get (count-1) elements from the stack starting from pos, count = 0 returns all elements >= pos
--  The element with the lowest position is at the top of the list
--  If the stack does contain less then count elements returns nil objects
--  Count == 1 means no elements will be returned
getElements :: (LuaStack a) => a -> Int -> Int -> [LuaObject]
getElements stack pos count
  --No elements
  | count == 1 = []
  --return whole stack starting at poos
  | count == 0 =
    map (\offset -> getElement stack (pos+offset)) [0..(stackSize stack - 1)]
  --return count-1 elements
  | count > 1 =
    map (\offset -> getElement stack (pos+offset)) [0..(count - 2)]



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
      Monad.foldM (\_ n -> print $ getElement stack n) () [0..stackSize stack]
