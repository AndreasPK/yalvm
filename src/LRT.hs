{-|
Module      : W
Description : Short description
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
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
