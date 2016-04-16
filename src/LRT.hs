module LRT(setGlobal, lrtPrint) where

import LuaObjects
import Parser
import LVM
import Control.Monad as Monad
import qualified Data.Map as Map

--add global values, names are to be given without trailing zero
setGlobal :: LuaState -> String -> LuaObject -> LuaState
setGlobal (LuaState exec globals) key value =
  LuaState exec $ Map.insert (key++"\NUL") value globals

definePrint :: IO LuaMap -> IO LuaMap
definePrint arguments = do
  a <- arguments
  mapM_ ((\ (LOString s) -> putStrLn s) . ltoString) (toList a)
  return $ createStack 1

lrtPrint = HaskellFunctionInstance "print" (createStack 0) definePrint
