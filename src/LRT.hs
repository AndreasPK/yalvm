module LRT(setGlobal, lrtPrint, registerAll, registerFunction) where

import LuaObjects
import Parser
import LVM
import Control.Monad as Monad
import qualified Data.Map as Map
import Control.Monad.ST as ST
import qualified Data.Foldable as Foldable

--add global values, names are to be given without trailing zero
setGlobal :: LuaState -> String -> LuaObject -> LuaState
setGlobal (LuaState exec globals) key value =
  let name = key ++ "\NUL"
  in
  LuaState exec $ Map.insert name value globals


registerFunction :: LuaState -> (IO LuaMap -> IO LuaMap) -> String -> LuaState
registerFunction state f name = setGlobal state name $ LOFunction $ HaskellFunctionInstance name (createStack 0) f


registerAll :: LuaState -> LuaState
registerAll s = registerFunction s definePrint "print"


definePrint :: (LuaStack stack) => IO stack -> IO LuaMap
definePrint arguments = do
  al <- toList arguments :: IO [LuaObject]
  Foldable.traverse_ (print . loString . ltoString) al
  createStack 1

lrtPrint = HaskellFunctionInstance "print" (createStack 0) definePrint
