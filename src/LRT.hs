module LRT(lrtPrint, registerAll, registerFunction) where

import LuaObjects
import Parser
import LVM
import Control.Monad as Monad
import qualified Data.Map as Map
import Control.Monad.ST as ST
import qualified Data.Foldable as Foldable
import Debug.Trace


registerFunction :: LuaState -> (LVStack -> IO LVStack) -> String -> IO ()
registerFunction state f name =
  writeGlobal (stateGlobals state) (name ++ "\NUL") $ LOFunction $ HaskellFunctionInstance name f
  --setGlobal state name $ LOFunction $ HaskellFunctionInstance name f


registerAll :: LuaState -> IO ()
registerAll s = registerFunction s definePrint "print"


definePrint :: (LuaStack stack) => stack -> IO LVStack
definePrint arguments = do
  al <- toList arguments :: IO [LuaObject]
  --print al
  putStrLn $ loString . ltoString $ head al --Foldable.traverse_ (putStrLn . loString . ltoString) al
  createStack 1

lrtPrint = HaskellFunctionInstance "print" definePrint
