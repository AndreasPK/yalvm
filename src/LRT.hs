module LRT(setGlobal, lrtPrint, registerAll, registerFunction) where

import LuaObjects
import Parser
import LVM
import Control.Monad as Monad
import qualified Data.Map as Map
import Control.Monad.ST as ST

--add global values, names are to be given without trailing zero
setGlobal :: LuaState -> String -> LuaObject -> LuaState
setGlobal (LuaState exec globals) key value =
  LuaState exec $ Map.insert (key++"\NUL") value globals

--
registerFunction :: LuaState -> (IO LuaMap -> IO LuaMap) -> String -> LuaState
registerFunction state f name = setGlobal state name $ LOFunction $ HaskellFunctionInstance name (createStack 0) f


registerAll :: LuaState -> LuaState
registerAll s = registerFunction s definePrint "print"


definePrint :: IO LuaMap -> IO LuaMap
definePrint arguments = do
  a <- arguments
  mapM_ ((\ (LOString s) -> putStrLn s) . ltoString) (toList a)
  return $ createStack 1

lrtPrint = HaskellFunctionInstance "print" (createStack 0) definePrint
