module LRT(lrtPrint) where

import LuaObjects
import Parser
import Control.Monad as Monad
import qualified Data.Map as Map
import Control.Monad.ST as ST
import qualified Data.Foldable as Foldable
import Debug.Trace



definePrint :: (LuaStack stack) => stack -> IO LVStack
definePrint arguments = do
  al <- toList arguments :: IO [LuaObject]
  --print al
  putStrLn $ loString . ltoString $ head al --Foldable.traverse_ (putStrLn . loString . ltoString) al
  createStack 1

lrtPrint = HaskellFunctionInstance "print" definePrint
